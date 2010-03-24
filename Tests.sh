#!/bin/bash
echo "---------INSERTTEST---------------"
./Main "insert node (<new att1='someval'>Content of new attribute</new>) as first into (/recipes/recipe/instructions)" examples/test.xml examples/inserttest.xml
echo
echo
echo "---------RENAMETEST---------------"
./Main "rename node (/recipes/recipe/ingredient[@amount=3]) as new" examples/test.xml examples/renametest.xml


