
<h1 align="center">Optimal Execution</h1>

The Server is the Backend
<details>
<summary>Stages</summary>

* Source to Tokens
* Tokens to AST
* AST to High Level Objects
* High Level Objects to typed Objects
* "Ownership" checker
* Interpreter

</details>

<br>
<br>

Each stage does diffrent checks. Errors are reported just by throwing a `LanguageException`, that contains the region and the file where the error occurred. This can be caught on the top level, to interface with js code in the future.

Syntax so far:
```

program ::= let* id!
let ::= "let" id! "="! expression!
expression ::= (id "+" id!) | id | number
 

id ::= "word"
number ::= "number" 

```

See [Test.txt](server/resources/test.txt) for a test file that will eval to 102