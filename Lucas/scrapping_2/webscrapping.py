#! /usr/bin/env python
# -*- coding: utf-8 -*-
# vim:fenc=utf-8

from bs4 import BeautifulSoup as BS
from requests import get
import re
import json

"""
Webscrapping step
"""

def traite_url(url, liste = None):
    page = get(url)
    code = page.content.decode("utf8")
    data = Jeu_video(code)
    if liste:
        liste.remove(url)
    with open(r'C:\Users\Lucas\Documents/M2/S2/Big data/Shiny_VG/Lucas/scrapping_2/data.json', 'a') as f:
        f.write(data.to_json())
        f.write(",\n")


class Jeu_video:
    """Classe pour gérer les données d'une page html descriptif d'un jeu vidéo"""

    def __init__(self, code_html):
        soupe = BS(code_html, features="lxml")
        self.set_titre(soupe)
        self.set_reviews(soupe)
        self.set_reste(soupe)
        self.set_desc(soupe)

    def __str__(self):
        return f"""
titre           : {self.titre}
reviews         : {self.reviews}
published_by    : {self.published}
developed by    : {self.developed}
released        : {self.released}
platform        : {self.platform}
genre           : {self.genre}
perspective     : {self.perspective}
gameplay        : {self.gameplay}
setting         : {self.setting}
narrative       : {self.narrative}
special edition : {self.edition}
add-on          : {self.add_on}
visual          : {self.visual}
misc            : {self.misc}
description     : {self.desc}
"""
    
    def set_titre(self, soupe):
        """Affecte le titre"""
        balise_titre, *_ = soupe.find_all(attrs = {'class': ['niceHeaderTitle']})
        self.titre = balise_titre.findChildren()[0].text
                                          
    def set_desc(self, soupe):
        """Affecte la description"""
        desc = ''
        boolean = False
        fenetre, *_ = soupe.find_all(attrs={'class': ['col-md-8 col-lg-8']})
        balise_desc = fenetre.find_all(attrs={'style': ["margin-top: 0"]})
        if balise_desc:
            if len(balise_desc) > 1:
                for texte in balise_desc:
                    desc += texte.text
            elif len(balise_desc) == 1:
                desc += balise_desc.text
        else:
            for enfant in fenetre.children:
                try:
                    d = str(enfant.text.lower())
                    if d == 'screenshots' or d == '[':
                        break
                    if boolean == True:
                        desc += d
                    if d == 'description':
                        boolean = True
                except AttributeError:
                    desc += enfant
        desc = desc.replace("[edit description | view history]","")
        desc = desc.replace("[add description]", "")
        self.desc = desc
    
    def set_reviews(self, soupe):
        fenetre = soupe.find_all(attrs={'class': ['reviewList table table-striped table-condensed table-hover']})
        motif_re = re.compile("^([0-9]|[1-9][0-9]|100)$")
        note = 0
        n = 0
        if fenetre:
            if len(fenetre) == 1:
                for critique in fenetre[0].find_all('td'):
                    if motif_re.findall(critique.text):
                        note = note + int(critique.text)
                        n = n + 1
            else:
                for critique in fenetre[1].find_all('td'):
                    if motif_re.findall(critique.text):
                        note = note + int(critique.text)
                        n = n + 1
            try: 
                moyenne = note / n
            except ZeroDivisionError:
                moyenne = None
        else: moyenne = None
        self.reviews = moyenne
        
    def set_reste(self, soupe):
        """Affecte le reste de la description du jeu"""
        self.published = None
        self.developed = None
        self.released = None
        self.platform = None
        self.genre = None
        self.perspective = None
        self.gameplay = None
        self.setting = None
        self.narrative = None
        self.edition = None
        self.add_on = None
        self.visual = None
        self.misc = None
        
        dictionnaire = dict()
        
        fenetre, *_ = soupe.find_all(attrs={'id': ['floatholder coreGameInfo']})
        left_part, *_ = fenetre.find_all(attrs={'id': ['coreGameRelease']})
        right_part, *_ = fenetre.find_all(attrs={'id': ['coreGameGenre']})
        
        informations = left_part.find_all('div')
        for i in range(0, len(informations)- 1, 2):
            dictionnaire[informations[i].text] = informations[i + 1].text
    
        informations = right_part.find_all('div')
        for i in range(0, len(informations)- 1, 2):
            dictionnaire[informations[i].text] = informations[i + 1].text
        for i in range(1, len(informations)- 1, 2):
            dictionnaire[informations[i].text] = informations[i + 1].text
        for clef in dictionnaire.keys():
            self.tri_reste(clef.lower(), dictionnaire[clef])
            
            
    def tri_reste(self, titre, valeur):
        if titre.replace(" ", "") == "publishedby":
            self.published = valeur.strip()
        elif titre.replace(" ", "") == "developedby":
            self.developed = valeur.strip()
        elif titre.replace(" ", "") == "released":
            self.released = valeur.strip()
        elif titre.replace(" ", "") == "genre":
            self.genre = valeur.strip()
        elif titre.replace(" ", "") == "perspective":
            self.perspective = valeur.strip()
        elif titre.replace(" ", "") == "gameplay":
            self.gameplay = valeur.strip()
        elif titre.replace(" ", "") == "setting":
            self.setting = valeur.strip()
        elif titre.replace(" ", "") == "specialedition":
            self.edition = valeur.strip()
        elif titre.replace(" ", "") == "add-on":
            self.add_on = valeur.strip()
        elif titre.replace(" ", "") == "narrative":
            self.narrative = valeur.strip()
        elif titre.replace(" ", "") == "visual":
            self.visual = valeur.strip()
        elif titre.replace(" ", "") == "misc":
            self.misc = valeur.strip()
        elif titre.replace(" ", "") == "platform":
            self.platform = valeur.strip()
        else:
            pass
    
    def to_json(self):
        """Renvoit une chaine pour stocker le résultat en json."""
        return json.dumps(self.__dict__)
    
        """        right_part = right_part.findChildren()[0]
        informations = right_part.find_all('div')
        for i in range(0, len(informations)- 1, 2):
            dictionnaire[informations[i].text] = informations[i + 1].text
        print(dictionnaire)
        for clef in dictionnaire.keys():
            self.tri_reste(clef.lower(), dictionnaire[clef])
            """