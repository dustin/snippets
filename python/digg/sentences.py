#!/usr/bin/env python
"""
Sentence analyzer.

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""

import re
import copy
import string

SENTENCE=re.compile("[\.\?\!]\s*")
WORD=re.compile("[,;]?\s*")
HTML=re.compile(r"<[^>]*>")
badchars="""\n\t#|{}()"'"""
TR=string.maketrans(badchars, ' ' * len(badchars))

def split_sentences(text):
    """Split a chunk of text into sentences."""
    stripped_text=HTML.sub('', text)
    return [s.strip() for s in SENTENCE.split(stripped_text) if s.strip() != '']

def normalize_sentence(s):
    """Normalize the words within a sentence."""
    return s.translate(TR).lower()

def split_words(s):
    """Split a sentence into a list of words."""
    return [w.strip() for w in WORD.split(s) if w.strip() != '']

def word_groups(words, size):
    """Construct word groups of a given size from the given list of words."""
    chunk=words[:size]
    words=words[size:]
    rv=[copy.copy(chunk)]
    while words:
        del chunk[0]
        chunk.append(words[0])
        del words[0]
        rv.append(copy.copy(chunk))
    return rv

def extract_word_groups(text, size):
    """Extract all of the word groups from the given text.

    text:  The given text (string)
    size:  The size of the word groups to extract.
    """
    rv=[]
    for s in map(normalize_sentence, split_sentences(text)):
        for g in word_groups(split_words(s), size):
            rv.append(g)
    return rv
