import urllib.request
import bs4
import ssl
import re
import sqlite3

conn = sqlite3.connect('dagstuhl.sqlite')
cur = conn.cursor()
cur.executescript("""
DROP TABLE IF EXISTS seminars;
CREATE TABLE seminars (
    seminar_id VARCHAR, 
    title VARCHAR, 
    date VARCHAR, 
    year INTEGER,
    PRIMARY KEY (seminar_id));
""")
conn.commit()

cur.executescript("""
DROP TABLE IF EXISTS participants;
CREATE TABLE participants (
    id INTEGER,
    seminar_id VARCHAR, 
    name VARCHAR, 
    affiliation VARCHAR, 
    country VARCHAR,
    PRIMARY KEY (id));
""")
conn.commit()

# 2001-2019 >> 01021 - 19512
# 2009: 09021
context = ssl._create_unverified_context()
for dagid in range(1021, 19512):
    id = str(dagid).zfill(5)
    print(f"Current Seminar ID: {id}")
    link = f"https://www.dagstuhl.de/no_cache/en/program/calendar/partlist/?semnr={id}"
    webpage=str(urllib.request.urlopen(link, context=context).read())
    soup = bs4.BeautifulSoup(webpage, 'lxml')

    h3 = soup.body.find('h3').text.split(",")
    if(len(h3)) > 1:
        print("Found participants list ...")
        date = h3[0].strip()
        year = h3[1].strip()
        title = (soup.body.find('h1').text.strip())

        cur.execute('INSERT INTO seminars (seminar_id, title, date, year) VALUES (?, ?, ?, ?)',
                    (id, title, date, year))
        conn.commit()

        participants = soup.body.find('div', attrs={'class':'participants'}).text
        p = (re.split('\)|\(|,', participants))
        p2 = [x.strip() for x in p]
        p = [re.sub('\[dblp\]', '', x) for x in p2]
        participants = []
        affiliations = []
        countries = []

        restart = True
        while restart:
            restart = False
            for i in range(2, len(p), 3):
                country = p[i]
                if len(country) != 2:
                    p.insert(i, "XX")
                    restart = True
                    break

        for i in range(2, len(p), 3):
            participants.append(p[i-2])
            affiliations.append(p[i-1])
            countries.append(p[i])

        for i in range(0, len(participants)):
            cur.execute('INSERT INTO participants (seminar_id, name, affiliation, country) VALUES (?, ?, ?, ?)',
                        (id, participants[i], affiliations[i], countries[i]))
            conn.commit()
