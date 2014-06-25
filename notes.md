iEvoBio June 24 2014
Brian O'Meara, Todd Vision, David Winter, Francois Michonneau
Use case: users have list of accessions
Alt use case: see which resources are used in a paper; decided to go with first

Code: https://github.com/bomeara/genbankcredit

Discussed biopython vs R. Most of us use R, users are there, went with R

Decided to modify read.GenBank from ape to return citation info

If PUBMED in record, pull that

Use table() at end to summarize everything

Status: can get pubmed ids back from read.GenBank. Working on getting references.

