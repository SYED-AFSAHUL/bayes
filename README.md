# Bayes

A naive Bayes classifier in Scala

## Training the classifier

```scala
import bayes._

Bayes.train("animals", "dogs go woof woof")
Bayes.train("sports", "I like playing basketball")

```

## Classifying text

Once we've trained our classifer we can use it to classify documents

```scala
Bayes.classify("I love to play basketball")
// res6: String = "sports"

Bayes.classify("When my dog barks he goes woof woof")
// res8: String = animals
```

