#! /usr/bin/env python
# -*- coding: utf-8 -*-
# vim:fenc=utf-8

from bs4 import BeautifulSoup as BS
import bs4
from requests import get
import json

"""
Webscrapping step
"""

def new_dict(dictionnaire):
    description, tableau = scrappe_info(dictionnaire)
    dictionnaire["Description"] = description
    dictionnaire["tableau"] = tableau
    with open(r'C:\Users\Lucas\Documents/M2/S2/Big data/VG_Shiny/Lucas/scrapping_3/vgsales2.json', 'a') as f:
        f.write(json.dumps(dictionnaire))
        f.write(",\n")

def scrappe_info(dictionnaire):
    titre = dictionnaire['Name'].lower().replace(" ", "+")
    url_recherche =  "https://en.wikipedia.org/w/index.php?search=" + titre + "&title=Special%3ASearch&go=Go&ns0=1"
    page = get(url_recherche)
    code = page.content.decode("utf8")
    soupe = BS(code, features="lxml")
    if soupe.find_all(attrs = {'id': ['coll-download-as-rl']}) and soupe.find_all(attrs = {'class': ['infobox hproduct']}):
        description = recherche_description(soupe)
        tableau = recherche_tableau(soupe)
    else:
        url_recherche =  "https://en.wikipedia.org/w/index.php?search=" + titre +"+game" + "&title=Special%3ASearch&go=Go&ns0=1"
        page = get(url_recherche)
        code = page.content.decode("utf8")
        soupe = BS(code, features="lxml")
        try:
            balise_lien, *_ = soupe.find_all(attrs = {'class': ['mw-search-result']})
            lien, *_  = balise_lien.find_all(href = True)
            page = get("https://en.wikipedia.org" + lien['href'])
            code = page.content.decode("utf8")
            soupe = BS(code, features="lxml")
            description = recherche_description(soupe)
            tableau = recherche_tableau(soupe)
        except ValueError:
            description = recherche_description(soupe)
            tableau = "NA"
    return description, tableau

def recupere_vrai_lien(soupe):

    page = get(url_recherche)
    code = page.content.decode("utf8")
    soupe = BS(code, features="lxml")
    

def recherche_description(soupe):
    try:
        page, *_ = soupe.find_all(attrs = {'class': ['mw-parser-output']})
        description = ""
        valeur = False
        for enfant in page.children:
            if type(enfant) == bs4.element.Tag:
                if enfant.find_all(attrs = {"class": ["mw-headline"]}):
                    if enfant.find_all(attrs = {"class": ["mw-headline"]})[0].text == "Gameplay":
                        valeur = True
                    else: 
                        valeur = False
                if valeur == True:
                    description = description + enfant.text + "\n"
    except ValueError:
        description = ""
    return description
                
    
def recherche_tableau(soupe):
    try:
        infobox, *_ = soupe.find_all(attrs = {'class': ['infobox hproduct']})
    except ValueError:
        infobox = "NA"
    return str(infobox)