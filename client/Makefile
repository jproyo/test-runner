../assets/index.html: src/TestRunnerApi.elm src/Main.elm
	elm make src/Main.elm
	mv index.html ../assets/

src/TestRunnerApi.elm: CompileToElm.hs ../server/src/App/Internal.hs
	(cd ../ ; stack exec -- runhaskell -iserver/src/Runner.hs client/CompileToElm.hs)

clean:
	rm -rf src/TestRunnerApi.elm

