# Commands

## installing and running hpt

`cabal install --lib lib:haskell-permission-tool --force-reinstall`

`cabal install hpt --overwrite-policy=always`

`hpt --help`

`hpt --neo4j --json subgraphs Main`


## Neo4j

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