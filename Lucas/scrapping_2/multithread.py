from multiprocessing.dummy import Pool as ThreadPool
import time
import requests
import webscrapping as wbscr
import json


def get_all_links():
    links = list()
    with open(r'C:\Users\Lucas\Documents/M2/S2/Big data/Shiny_VG/Lucas/scrapping/good_url.json',"r",encoding="utf-8") as fichier:
        urls = fichier.read()
    links = json.loads(urls)
    return links


def get_response(url):
    wbscr.traite_url(url)


def multi_thread():
    start_time = time.time()

    # make the Pool of workers
    pool = ThreadPool(180)

    # read the handles.txt file for instagram handles
    links = get_all_links()

    # call the start function in different thread for each handle
    pool.map(get_response, links)
    # print(results)

    # close the pool and wait for the work to finish
    pool.close()
    pool.join()
