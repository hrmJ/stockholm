#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import pypandoc
import glob
import os
import re

def ConvertToTxt(path, txt_path):
    """
    Convert all word files to plain text
    path: the location of the folder with the examples
    txt_path: path to the folder where the txt files will be saved (e.g. c:\Documents\examples_txt)
    """
    if path[-1] == "/":
        path = path[:-1]
    if txt_path[-1] == "/":
        txt_path = txt_path[:-1]

    for papka in glob.glob(path + "/*"):
        adj = papka[papka.rfind("/")+1:].lower()
        folder_name = txt_path + "/" + adj
        if not os.path.exists(folder_name):
            os.makedirs(folder_name)
        print(adj)
        for word_file in glob.glob(papka + "/*docx"):
            fname = word_file[word_file.rfind("/")+1:].replace(".docx","")
            print(fname)
            outfile = "{}/{}/{}.txt".format(txt_path, adj,fname)
            if not os.path.isfile(outfile):
                # Only try conversion if there this file has not been converted before
                try:
                    output = pypandoc.convert_file(word_file, 'plain', outputfile=outfile)
                except:
                    print("Error: " + fname)


def ConvertToList(path):
    """
    Convert the text files to lists
    path: the location of the folder with the examples
    """
    examples = list()
    for papka in glob.glob(path + "/*"):
        adj = papka[papka.rfind("/")+1:].lower()
        for txt_file in glob.glob(papka + "/*"):
            fname = txt_file[txt_file.rfind("/")+1:].replace(".txt","")
            pat = re.compile(r"\s*(lubim|mil|dorog|rodn|любим|мил|дорог|родн)(ie|ой|ый|ая|ые|oj|aja|yje|ye|oe|y1|yj|yyj)\s*",re.IGNORECASE)
            participants = pat.sub(" [adj] ",fname).strip().replace(" vok","").replace("vok ","").replace("vokativ"," ")
            participants = participants.replace("vokativ"," ")
            participants = participants.replace(" rodoj"," [adj]")
            participants = participants.replace(" dorogj"," [adj]")
            with open(txt_file,"r") as f:
                file_content = f.read()
            contexts = re.split(r"\n{2,}", file_content)
            for idx, context in enumerate(contexts):
                try:
                    year = re.search(r"\((\d+)[\d-]*\)\s*\]",context).group(1)
                except AttributeError:
                    year = "?"
                if year == "?":
                    try:
                        year = re.search(r"(\d+)\s*\]",context).group(1)
                    except AttributeError:
                        year = "?"
                participants = participants.lower()
                participants = re.sub("nkrj","",participants)
                participants = re.sub(r"\s+"," ",participants)
                participants = re.sub(r"(моя|moja)","moj",participants)
                participants = re.sub(r"(мой)","moj",participants)
                #print(participants)
                examples.append({"context":context,"adj":adj,"participants":participants.strip(),"year":year,"pattern":"[adj]","other":participants,
                    "tonalnost": "agress" if "agress" in participants else "other" })
    return examples

#ConvertToList("/home/juho/data/stockholm/txt/")
