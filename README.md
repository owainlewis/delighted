# Delighted

A Haskell client for working with the Delighted.com API. You will need an API key to use this library.

![](https://dcx14qs33eg2z.cloudfront.net/assets/logos-s5d111f3d38-cec0f8af4719c5b72440a3fbb849ca66.png)

# Add Person

```haskell

import qualified Network.Delighted.API as Delighted
import qualified Data.Map as M

Delighted.createPerson "APIKEY" $ M.fromList [ ("email", "owain.lewis@owainlewis.com") ]
```

# Get Survey Repsonses

```haskell

import qualified Network.Delighted.API as Delighted
import qualified Data.Map as M

λ> Delighted.getSurveyResponses key M.empty
Just [ 
  SurveyResponse { 
    responseId = "13133762",
    surveyPersonId = "52830503",
    score = 10,
    comment = Just "Great product"}
  ]
```

# Get Metrics

```haskell
import qualified Network.Delighted.API as Delighted
import qualified Data.Map as M

Delighted.getMetrics M.empty
```
