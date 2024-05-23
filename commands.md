# Commands

## Installing and running hpt

`cabal install --lib lib:haskell-permission-tool --force-reinstall`

`cabal install hpt --overwrite-policy=always`

`hpt --help`

`hpt --neo4j --json /path/to/subgraphs/ --trim --rm Main`


# Neo4j

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