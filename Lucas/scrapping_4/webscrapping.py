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
    description, photo, width, height = scrappe_info(dictionnaire)
    dictionnaire["Description"] = description
    dictionnaire["photo"] = photo
    dictionnaire["width"] = width
    dictionnaire["height"] = height
    with open(r'C:\Users\Lucas\Documents/M2/S2/Big data/VG_Shiny/Lucas/scrapping_4/desc_publisher.json', 'a') as f:
        f.write(json.dumps(dictionnaire))
        f.write(",\n")

def scrappe_info(dictionnaire):
    titre = dictionnaire['Publisher'].lower().replace('\xa0', ' ').replace(" ", "+")
    url_recherche =  "https://en.wikipedia.org/w/index.php?search=" + titre + "&title=Special%3ASearch&go=Go&ns0=1"
    page = get(url_recherche)
    code = page.content.decode("utf8")
    soupe = BS(code, features="lxml")
    if soupe.find_all(attrs = {'id': ['coll-download-as-rl']}):
        description = recherche_description(soupe)
        photo, width, height  = recherche_img(soupe)
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
            photo, width, height = recherche_img(soupe)
        except ValueError:
            description = recherche_description(soupe)
            photo, width, height = None, None, None
    return description, photo, width, height


def recherche_description(soupe):
    valeur = False
    description = ""
    try:
        page, *_ = soupe.find_all(attrs = {'class': ['mw-parser-output']})
        for enfant in page.children:
            if type(enfant) == bs4.element.Tag:
                balise_avant = enfant.find_all(attrs = {"class": ["infobox-title"]})
                if balise_avant:
                    valeur = True
                if enfant.find_all(attrs = {'class': ['toctitle']}):
                    valeur = False
                if valeur == True and not balise_avant:
                    description = description + " \n" + enfant.text
    except ValueError:
        description = ""
    return description
                
    
def recherche_img(soupe):
    try:
        tag_photo, *_ = soupe.find_all(attrs = {'class': ['infobox vcard']})
        img, *_ = tag_photo.find_all('img')
        photo, width, height = str(img['src']), img['width'], img['height']
    except ValueError:
        photo, height, width = None, None, None 
    return photo, width, height