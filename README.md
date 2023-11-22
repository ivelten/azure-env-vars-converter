# Azure Environment Variables Converter
This is a simple console built in Haskell to convert azure environment variables to a `local.settings.json` file.

The format of the input is the JSON returned at the "Advanced Edit" option in azure environment variables panel.

## How to build this project

- Install GHC and Cabal. Easiest way may be using [GHCup](https://www.haskell.org/ghcup/).
- Go to your application in Azure Portal and click in the "Advanced Edit" in the "Environment Variables" section.
- Copy the JSON text from the Advanced Edit section, create a file "input.json" in the project root folder and paste it inside. Save the file.
- Run the project using `cabal run`.
- The output file will be generated in the project root folder, named `local.settings.json`.
