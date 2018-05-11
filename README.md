TOPSEQ: Topics Sequence Finder
==============================
This script locates sequences in raw topics code files. For more information, see *"Compliance Chains from BONDS TOPICS micro social coding"* by Kristin Nordahl.

Make note of the following premises:
* P1: When multiple start codes occur before a stop code, the last start code is kept. The previous are discarded.
* P2: When multiple stop codes occur, only the first one counts. The latter are discarded.


## Prerequisites:
You need The Haskell Tool Stack >= Version 1.6.5. Download the latest version here: https://docs.haskellstack.org/

## Usage

**Windows:**
`> stack tsf.hs`

**Linux:**
`> ./tsf.hs`

You  must modify the script to get what you want. Sequences are defined like this:

```haskell
seqStart = RecodeSpec (Initiator Parent) [ContentCode "42"] [Valence "1", Valence "2", Valence "3"]
seqSstop = RecodeSpec (Initiator Child) [ContentCode "01"] [] -- empty valence == any valence
rseq = RecodeSequence seqStart seqStop (WindowSizeInSecs 6)
```
