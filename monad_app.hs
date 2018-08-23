import Data.Maybe
import Data.Char
import Control.Applicative 
import Control.Monad 

{-
      -----------------------------------
        Part 1: Monads vs Applicatives
        a. Monads are Applicatives
      -----------------------------------

     Let Ctl be a wrapper of (computation) values, which can be either errors 
     or Computations
-}
data Ctl a  = Err | Compute a

instance Functor Ctl where
  fmap f Err = Err 
  fmap f (Compute y) = Compute (f y)

{- 
      Suppose we would like to sequence computation values, hence make Ctl monadic
-}
instance Monad Ctl where
  return = Compute
  Err >>= f = Err   -- error message
  (Compute y) >>= f = (f y)   -- continue sequencing via f

  {-
  Example 1 (addition):
  do{
      x <- (Compute 1);
      y <- (Compute 2);
      return (x+y)
  } :: Ctl Integer

  Here, two computations are combined in a single one;

  Example 2 (transformation):
  do {
      x <- (Compute 1);
      return [x]
  } :: Ctl [Integer]
  
  A computation which returns an Integer is transformed into one which returns a 
  list of integers

  Example 3 (breaking sequence) - add up two computations and then insert them in the
  empty list:

  do {
      x <- do {
          y <- (Compute 1);
          z <- Err;
          return (y + z)
      }
      return [x]
  } :: Ctl a

  Here, the sequence of computations has been broken by Err. 
  The last computation is not performed
  -}



{-
    In this particular case, we can achieve the same sequencing, by making Ctl Applicative:
-}

instance Applicative Ctl where
  pure = Compute
  (Compute f) <*> (Compute x) = Compute (f x)
  (Compute f) <*> Err = Err 
  Err <*> (Compute _) = Err
  Err <*> Err = Err   

  {-
  Example 1 (addition) :
  pure (+) <*> (Compute 1) <*> (Compute 2) =
  Compute (+ 1) <*> (Compute 2) =
  Compute (+ 1 2)


  Example 2 (transformation) :
  pure (\x->[x]) <*> (Compute 1)

  Example 3 (breaking sequence):

  pure (\x->[x]) <*> (pure (+) <*> Err <*> (Compute 1))


  So far we have seen that sequencing computations in our case, can be obtained
  via the monadic binding operator (>>=) as well as via the applicative <*> operator

  In fact, in the general case, >>= is a specialisation of <*>, 
  and <*> can be implemented in terms of >>=. 

  Let us call it 'ap' in what follows:

  ap :: Monad t => t (a -> b) -> t a -> t b

  ap mf m = do {
              f <- mf;
              x <- m;
              return f x       
           } 

  Therefore, if t is a Monad (and >>= is implemented), the ap (defined in the Monad class)
  comes for free. In other words, if t is a Monad, then t is an Applicative.

  Note that <*> and ap are not the very same function. For this very reason, in newer GHC
  versions, <*> IS REQUIRED to be the same as ap, by enforcing that Monads are Applicatives.



  b. Applicatives are not necessarily Monads
  ------------------------------------------------


  Consider the following strategy for sequencing computations:
     - Compute x
     - if x> 0 then Compute y, else Compute z

  We can easily specify this strategy, if Ctl is a Monad:

    do { x <- Compute 1;
         if x>0 then Compute 2 else Compute 3
         }     

  Unfortunately, there is no way of specifying PRECISELY the same strategy,
  if Ctl is (ONLY) Applicative.

  The closest we can get, is:

    pure (\x th el->if x>0 then th else el) <*> Compute 1 <*> Compute 2 <*> Compute 3

  Note that, unlike the previous variant, ALL THREE computations are performed, 
  before the if is actually evaluated.

  The takeaway here is that, while Applicatives model objects on which a sequence of
    operations can be performed, this sequence is HARD-CODED, and cannot be altered while
    operations are performed.

  With Monads, this is indeed possible.




      ------------------------------------------------
        Part 2: Applicatives (and Monads) vs Functors
      ------------------------------------------------

      It is easy to show that an Applicative (or Monad) is a Functor:

      fmap :: (a -> b) -> t a -> t b

      fmap f o = (pure f) <*> o     -- hence if t is Applicative, it is also a Functor

      fmap f o = do {               -- also, if t is a Monad, it is also a Functor
                 x <- o;
                 return f x;
              }

      As an exercise, we show that Haskell lists are Monads, as well as Applicatives (hence also Functors):

      instance Monad [] where
          return x = [x]
          (here, (>>=) :: [a] -> (a -> [b]) -> [b])
          l >>= f = concat (map f l)

      instance Applicative [] where
          pure x = [x]
          (here, <*> :: [a->b] -> [a] -> [b])
          f <*> x = zipWith (\g y->g y) f x


      Finally, we look at Functors which cannot be Applicative.

      Consider:
  -}
data Pair a b = Pair a b

{- The type Pair a :: * => * (i.e. pairs where the first elem is of type 'a') can be a Functor: -}

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x (f y)

{- but there is no way in defining <*> in such a way that the first element(s) of the pair
   are not treated as residual(s). For instance, below, what strategy should be enforced 

   (Pair x f) <*> (Pair x' y) = Pair ? (f y)

   in order to compute the first element of the resulting pair?


   Note however, that Pairs can be made Applicative, as follows:

   instance Monoid a => Functor (Pair a) where
      pure = Pair mzero 
      (Pair x f) <*> (Pair x' y) = Pair (x <> x') (f y)

  In this case, the type a is not arbitrary, but a Monoid (hence it has a mzero element,
  and a binary operation <>)

  More concretely, the type a could be a String:

  instance Functor (Pair String) where
    pure = Pair ""
    (Pair s f) <*> (Pair s' y) = Pair (s++s') (f y)   


      ------------------------------------------------
        Part 3: What is not a Functor?
      ------------------------------------------------

  Consider the following implementation of sets, via a characteristic function (a predicate
  which checks which element is in the set):
-}

data Set a = Set (a -> Bool)

{-
  There is no way to write a function of signature (not a more particular one):
  fmap :: (a -> b) -> (a -> Bool) -> (b -> Bool)

  that satisfies the axioms of fmap:
  (A1) fmap id  ==  id
  (A2) fmap (f . g)  ==  fmap f . fmap g

  (This should be nice to prove - have not searched for the proof yet)

  Interestingly, we can define:

  ffmap :: (a -> b) -> (b -> Bool) -> (a -> Bool)

  as follows:
  
  fmap f set = f . set

-}




