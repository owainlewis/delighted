# Delighted

A Haskell client for working with the Delighted.com API. You will need an API key to use this library.

# Add Person

```haskell

import qualfied Network.Delighted.API as Delighted

Delighted.createPerson key $ M.fromList [ ("email", "owain.lewis@owainlewis.com") ]
```

# Get Survey Repsonses

```haskell
λ> getSurveyResponses key M.empty
Just [ SurveyResponse { responseId = "13133762",
                        surveyPersonId = "52830503",
                        score = 10,
	                comment = Just "Great product"}
     ]
```

# Get Metrics

```haskell

getMetrics M.empty
```