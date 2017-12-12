#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import pypandoc
import glob
import os
import re

def ConvertToTxt(path):
    """
    Convert all word files to plain text
    path: the location of the folder with the examples
    """
    for papka in glob.glob(path + "/Beispiele/*"):
        adj = papka[papka.rfind("/")+1:].lower()
        folder_name = "Beispiele_txt/" + adj
        if not os.path.exists(folder_name):
            os.makedirs(folder_name)
        print(adj)
        for word_file in glob.glob(papka + "/*docx"):
            fname = word_file[word_file.rfind("/")+1:].replace(".docx","")
            print(fname)
            try:
                output = pypandoc.convert_file(word_file, 'plain', outputfile="Beispiele_txt/{}/{}.txt".format(adj,fname))
            except:
                print("Error: " + fname)


def ConvertToList(path):
    """
    Convert the text files to lists
    path: the location of the folder with the examples
    """
    examples = list()
    for papka in glob.glob(path + "/Beispiele_txt/*"):
        adj = papka[papka.rfind("/")+1:].lower()
        for txt_file in glob.glob(papka + "/*"):
            fname = txt_file[txt_file.rfind("/")+1:].replace(".txt","")
            pat = re.compile(r"\s*(lubim|mil|dorog|rodn|любим|мил|дорог|родн)(ой|ый|ая|ые|oj|aja|yje|yj)\s*",re.IGNORECASE)
            pattern = pat.sub(" [adj] ",fname).strip().replace(" vok","")
            with open(txt_file,"r") as f:
                file_content = f.read()
            contexts = re.split(r"\n{2,}", file_content)
            for idx, context in enumerate(contexts):
                try:
                    year = re.search(r"\((\d+)[\d-]*\)\s*\]",context).group(1)
                except AttributeError:
                    year = "?"
                examples.append({"context":context,"adj":adj,"pattern":pattern,"year":year})
            break
    return examples

