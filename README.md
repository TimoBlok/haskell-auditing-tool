
# `haskell-permission-tool` - analyse function dependencies in Haskell source code

Tested with GHC-9.4.8

## Introduction

This package contains a tool which can be used to generate the call graph of a haskell program. the executable can do some preliminary filtering and can export the graph, for example, to neo4j for fine-grained querying.

## Usage

### Enable plugin

GHC allows a user to enable a plugin with the `-fplugin` flag, which lets te user list the module containing the plugin. This is easily done in a .cabal file. Here, you only need to make sure you add the plugin package to the `build depends` section. The problem with this however, is that this will only apply the plugin on `local` packages, but in order to get a full picture of what the program is doing, you might want to run it on the projects `external` dependencies as well. Luckily, we can do this with a `cabal.project` file.

However, using a `cabal.project` file for plugins is slightly less straight forward. This is because we will have to talk directly with ghc, instead of letting cabal solve were to find the plugin. This means we have to bring the plugin in scope ourselves. This requires us to state the package database (folder of installed packages), and then which package the plugin resides in, using the flags `-package-db` and `-plugin-package` respectively.

Example cabal.project file

```cabal
packages:
  .

package *
  ghc-options: 
    -package-db /global/path/to/cabal/store/ghc-version/package.db/
    -plugin-package=haskell-permission-tool
    -fplugin=Plugin.AnalysisPlugin
    -fplugin-opt=Plugin.AnalysisPlugin:/global/path/to/outputfolder
```

The plugin expects a directory as input using the `-fplugin-opt` flag, this will be where the plugin outputs all the subgraphs data in json files.

# Commands

## Installing and running hpt

Installing the Plugin: `cabal install --lib lib:haskell-permission-tool --force-reinstall`

Installing the Executable: `cabal install hpt --overwrite-policy=always`

## using hpt

`hpt --help`

using neo4j and trimming to just include paths eminating from the main module:

`hpt --neo4j --json /path/to/subgraphs/ --trim --rm Main`

using hpt to track differences in IO permissions
`hpt --json /path/to/subgraphs/ --trim --filter-io --query some-package:Data.Module1:function`

this outputs a text file, and if you run this command everytime you build and there is a difference, you might want to check it out


## Installing and running neo4j with nix

```
NEO4J_HOME=$(mktemp -d) nix run nixpkgs#neo4j --impure console
```

```
nix run nixpkgs#firefox
```

## Example Cypher queries

To find the subgraph below a certain declaration:

```
MATCH (_:Declaration {name:"exampleFunction"})-[*0..]->(decl) 
RETURN decl
```

Or you can be more specific of course:


```
MATCH (saveP:Declaration {name:"exampleFunction", module:"ExampleModule", unit:"example-package"})-[*0..]->(decl) 
RETURN decl
```

If you only want to know which permissions a certain declaration needs (aka the leaves below that declaration):


```
MATCH (saveP:Declaration {name:"exampleFunction", module:"ExampleModule", unit:"example-package"})-[*0..]->(decl)
WHERE Not (decl)-->() 
RETURN decl
```

Find all paths that lead from example-package to a certain node:

```
MATCH (decl:Declaration {unit:"example-package"})-[*0..]->(n:Declaration {name:"exampleName"})
RETURN decl
```

```
MATCH (decl:Declaration)-[*0..]->(n:Declaration)
where n.unit =~ "example-pack.*"
RETURN decl
```

## Related

- Parts of the code base are inpsired by [cabal-audit](https://github.com/TristanCacqueray/cabal-audit/tree/main)
- [calligraphy](https://hackage.haskell.org/package/calligraphy)
- [graph-trace](https://hackage.haskell.org/package/graph-trace)

## Special thanks

Special thanks to Lawrence Chonavel for letting me use his project as a template.
