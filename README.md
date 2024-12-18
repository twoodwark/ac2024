
### Usage

```
function ad() { set -x ; cmd="$*"; pbpaste | sbt "runMain $cmd" }
ad Day1
```
