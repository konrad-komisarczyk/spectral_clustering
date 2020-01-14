# Generowanie zbioru testowego cities
# PDU 18/19 - PD nr 2
# autor: Konrad Komisarczyk

from bs4 import BeautifulSoup
import requests

html = requests.get("https://pl.wikipedia.org/wiki/Miasta_w_Polsce").content
soup = BeautifulSoup(html)

coords_file = open('coords', 'w+')

for div in soup.find_all('div'):
    if div.get("style") == '-moz-column-count:3; -webkit-column-count:3; column-count:3;':
        for li in div.find_all('li'):
            for a in li.find_all('a'):
                nazwa = a.get("title")
                #print(nazwa)
                link = a.get("href")
                #print(link)
                link_html = requests.get("https://pl.wikipedia.org" + link).content
                link_soup = BeautifulSoup(link_html)
                lat = []
                lon = []
                for span in link_soup.find_all('span'):
                    if span.get("class") == ["latitude"]:
                        lat.append(span.get_text())
                    if span.get("class") == ["longitude"]:
                        lon.append(span.get_text())
                print(nazwa, lat[1], lon[1], sep=";", file=coords_file)

link2 = "https://pl.wikipedia.org/wiki/Dane_statystyczne_o_miastach_w_Polsce"
html2 = requests.get(link).content
soup2 = BeautifulSoup(html2)

population_file = open('population', 'w+')

for tr in soup2.find_all('tr'):
    for td in tr.find_all('td'):
        print(td.get_text()[0:-1], file=population_file, end=';')
    print('', file=population_file)