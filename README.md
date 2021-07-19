shopping-cart-haskell
=====================

[![CI Status](https://github.com/gvolpe/shopping-cart-haskell/workflows/Haskell%20CI/badge.svg)](https://github.com/gvolpe/shopping-cart-haskell/actions)

Haskell version of the Shopping Cart developed in the book [Practical FP in Scala](https://leanpub.com/pfp-scala).

### How to run

Within a [Nix](https://nixos.org/) shell (run `nix-shell` - recommended), follow the commands below.

#### Run web application

```
cabal new-run shopping-cart
```

#### Run tests

```
cabal new-run shopping-cart-tests
```

## Comparison with the Scala application

The [original version](https://github.com/gvolpe/pfps-shopping-cart) of the Shopping Cart has been written in [Scala](https://www.scala-lang.org/). The Haskell application's design is quite similar.

- Services are represented using polymorphic records of functions.
- The Newtypes / [Refined](https://hackage.haskell.org/package/refined) duo is used for strongly-typed data.
- [Retry](https://hackage.haskell.org/package/retry) is used for retrying computations compositionally.
- [Servant](https://hackage.haskell.org/package/servant) is used as the default HTTP server.
- [Wreq](https://hackage.haskell.org/package/wreq) is used as the default HTTP client.
- [Hedis](https://hackage.haskell.org/package/hedis) is used as the default Redis client.
- [PostgreSQL Simple](https://hackage.haskell.org/package/postgresql-simple) + [Raw Strings QQ](https://hackage.haskell.org/package/raw-strings-qq) are used to handle PostgreSQL stuff.

A polymorphic record of functions looks as follows:

```haskell
data Brands m = Brands
  { findAll :: m [Brand]
  , create :: BrandName -> m ()
  }
```

Whereas in Scala, we represent it using `trait`s (although `case class` / `class`es would work too):

```scala
trait Brands[F[_]] {
  def findAll: F[List[Brand]]
  def create(name: BrandName): F[Unit]
}
```

We pass them along as explicit dependencies, though, so we don't treat them as typeclasses. In Scala, we use the same encoding for typeclasses (and then pass them *implicitly*) but in Haskell the encoding differs:

```haskell
class Brands m where
  findAll :: m [Brand]
  create :: BrandName -> m ()
```

Typeclasses were used to encode effects such as `Background`, `Logger` and `Retry`:

```haskell
class Background m where
  schedule :: m a -> Minutes -> m ()

instance Background IO where
  schedule fa mins = void $ async (threadDelay (microseconds mins) >> fa)
    where microseconds Mins {..} = 60000000 * unrefine unMins
```

In Scala, this is how it is encoded:

```scala
trait Background[F[_]] {
  def schedule[A](fa: F[A], duration: FiniteDuration): F[Unit]
}

object Background {
  def apply[F[_]: Background]: Background[F] = implicitly

  implicit def bgInstance[F[_]](implicit S: Supervisor[F], T: Temporal[F]): Background[F] =
    new Background[F] {
      def schedule[A](fa: F[A], duration: FiniteDuration): F[Unit] =
        S.supervise(T.sleep(duration) *> fa).void
    }
}
```

Having an implicit implementation is how we define coherent instances, though, they can be easily overridden (but this is also possible in Haskell using the right extensions).

### Missing features

There are some features I don't plan to implement due to lack of time and motivation.

- JWT Authentication: if you want to get it done, here's some nice [documentation](https://docs.servant.dev/en/stable/cookbook/jwt-and-basic-auth/JWTAndBasicAuth.html) on how to get started.
- Configuration: I recommend using [Dhall](https://dhall-lang.org/). You can have a look at how it's done [here](https://github.com/gvolpe/musikell).
- Tests: the machinery is in place but it's a ton of work to write tests. I'll leave that for another day :)
