#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue May  1 01:49:46 2018

@author: chan
"""


import requests, xmltodict, ast, re
import numpy as np
import pandas as pd
import tushare as ts


def getPageHTML(typCode, page):
    url = 'http://q.jrjimg.cn/'
    para = {'q': 'cn|s|bk' + typCode,
            'c': 'm',
            'n': 'hqa',
            'o': 'pl,d',
            'p': str(page) + '050',
            }
    r = requests.get(url, params = para)
    t = r.text
    return t
    

def getPageTable(txt):
    txt = txt[txt.find('{'):-2]
    dic = eval(txt, type('Dummy', (dict,), dict(__getitem__=lambda s, n: n))())
    tab = [x[1] for x in dic['HqData']]
    return tab


def getIndustry(industry):
    ans = []
    pg = 1
    while True:
        tab = getPageTable(getPageHTML(industry, pg))
        if len(tab) == 0:
            break
        pg += 1
        ans += tab
    return ans


def getHS300List():
    hs300 = np.array(ts.get_hs300s()['code']).tolist()
    hs300.sort()
    return set(hs300)


if __name__=='__main__':
    
    hs300 = getHS300List() # set
    
    code = [['医疗保健', '400116778'],
            ['能源', '400116731'],
            ['公用事业', '400116686'],
            ['金融', '400116539'],
            ['原材料', '400116597'],
            ['工业', '400116601'],
            ['非必需消费品', '400116762'],
            ['地产业', '400130252'],
            ['电信服务', '400116683'],
            ['信息技术', '400116672'],
            ['必需消费品', '400116774']
           ]
    
    ans = []
    for c in code:
        tab = getIndustry(c[1])
        for t in tab:
            if t in hs300:
                ans.append([t,c[0]])
    
    df = pd.DataFrame(ans)
    df.columns = ['code', 'industry']
    df.to_csv('industry.csv', index = None, encoding = 'utf-8-sig')
    
    
    
    
    
    
    
    
    
    
    
    
    
    


