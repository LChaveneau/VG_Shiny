from multiprocessing.dummy import Pool as ThreadPool
import time
import requests
import webscrapping as wbscr
import json
import csv


def get_all_lines():
    liste = []
    with open(r'C:\Users\Lucas\Documents/M2/S2/Big data/VG_Shiny/Lucas/scrapping_4/publisher_to_scrap.csv', 'r') as data:
        for line in csv.DictReader(data):
            liste.append(line)
    return liste


def get_response(lines):
    wbscr.new_dict(lines)


def multi_thread():
    start_time = time.time()

    # make the Pool of workers
    pool = ThreadPool(180)

    # read the handles.txt file for instagram handles
    lines = get_all_lines()
    # call the start function in different thread for each handle
    pool.map(get_response, lines)
    # print(results)

    # close the pool and wait for the work to finish
    pool.close()
    pool.join()
