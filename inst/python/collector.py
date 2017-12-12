import pypandoc
import glob
import os
import re

def ConvertToTxt():
    #Convert all word files to plain text
    for papka in glob.glob("Beispiele/*"):
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


def ConvertToList():
    """Convert the text files to lists"""
    examples = list()
    for papka in glob.glob("Beispiele_txt/*"):
        adj = papka[papka.rfind("/")+1:].lower()
        for txt_file in glob.glob(papka + "/*"):
            fname = txt_file[txt_file.rfind("/")+1:].replace(".txt","")
            pat = re.compile(r"\s*(lubim|mil|dorog|rodn|любим|мил|дорог|родн)(ой|ый|ая|ые|oj|aja|yje|yj)\s*",re.IGNORECASE)
            pattern = pat.sub(" [adj] ",fname).strip()
            with open(txt_file,"r") as f:
                file_content = f.read()
            contexts = re.split(r"\n{2,}", file_content)
            for idx, context in enumerate(contexts):
                print("{}: {}/{}".format(adj, idx+1, len(contexts)))
                examples.append({"context":context,"adj":adj,"pattern":pattern})

ConvertToList()
