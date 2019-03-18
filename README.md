 ![version: 0.1](https://img.shields.io/static/v1.svg?label=version&message=0.1&color=lightgrey) ![local-build: passing](https://img.shields.io/static/v1.svg?label=local-build&message=passing&color=green) ![local-tests: passing](https://img.shields.io/static/v1.svg?label=local-tests&message=passing&color=green) ![statement-coverage: 38.3%](https://img.shields.io/static/v1.svg?label=statement-coverage&message=38.3%&color=orange)  ![branch-coverage: 54.67%](https://img.shields.io/static/v1.svg?label=branch-coverage&message=54.67%&color=yellow)

  *Tested locally on x86_64 GNU/Linux
 with `scalaVersion = 2.12.6`, `sbtVersion = 1.1.4`. Readme generated on 2019-03-18.* 

# forest-ml parser

Grammar and custom parser combinators for `forest-ml` (a tiny
textual tree serialization / markup language).
Parser combinators can fall back to context-sensitive "parentheses egypticity"
for better error reporting.
Purely experimental, currently no usable `main`.
