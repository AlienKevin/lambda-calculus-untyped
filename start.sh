sed -i 's+src="elm.js"+src="/public/elm.js"+' public/index.html
elm-live src/LambdaRepl.elm --start-page public/index.html --host 0.0.0.0 -- --output=public/elm.js