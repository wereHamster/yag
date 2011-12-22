Yet Another Git Clone -- Written in Haskell
-------------------------------------------

I write this mainly for educational purposes, to learn Haskell.

What it can:

- Read lose objects from the .git/objects directory.
- Parse git objects (using attoparsec).
- Serialize objects (commit, blob and tag; tree not yet finished)
- Revision parsing (like git rev-parse)
- Gitdir discovery


What's missing:

- Error handling
- Config file parser
- Packfile reader/parser
- Writing lose objects
- Index (reading/writing/updating)
- Porcelain (at least show and log would be useful)
- Cleanup Lazy/Strict ByteString handling, BS <-> String conversions etc.
