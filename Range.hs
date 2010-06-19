module Range where

data Endpoint = Inc Rational | Exc Rational

-- range (a, b) is the list of integers greater than a and less than b,
-- inclusive or exclusive as specified
range :: (Endpoint, Endpoint) -> [Integer]
range (a, b) = [lower..upper] where
  lower = case a of
    Inc r -> ceiling r
    Exc r -> floor r + 1
  upper = case b of
    Inc r -> floor r
    Exc r -> ceiling r - 1
