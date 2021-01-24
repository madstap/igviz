.PHONY: test deploy

test:
	clojure -Srepro -M:test

recex.jar: src/**/*
	clojure -Srepro -M:jar

pom.xml: deps.edn
	clojure -Srepro -Spom

deploy: pom.xml test recex.jar
	clojure -Srepro -X:deploy
