import pandas as pd
import json
import os
import sys
import matplotlib.pyplot as plt
from textwrap import wrap

root_dir = sys.argv[1]

method_rows = []
type_rows = []
ctors = {}

def readProperty(element, typ, searcher, timeout):
    for child in element.get('Children', []):
        if (child['Kind'] == 'PropertyGetter'):
            method_rows.append([searcher, timeout, typ, f"{element['Name']}.get", child['CoveragePercent'], child['TotalStatements']])
        elif (child['Kind'] == 'PropertySetter'):
            method_rows.append([searcher, timeout, typ, f"{element['Name']}.set", child['CoveragePercent'], child['TotalStatements']])
                
def readEvent(element, typ, searcher, timeout):
    for child in element.get('Children', []):
        if (child['Kind'] == 'EventAdder'):
            method_rows.append([searcher, timeout, typ, f"{element['Name']}.add", child['CoveragePercent'], child['TotalStatements']])
        elif (child['Kind'] == 'EventRemover'):
            method_rows.append([searcher, timeout, typ, f"{element['Name']}.remove", child['CoveragePercent'], child['TotalStatements']])

def readElement(element, typ, namespace, searcher, timeout):
    if (element['Kind'] == 'Property' or element['Kind'] == 'AutoProperty' or element['Kind'] == 'Indexer'):
        readProperty(element, typ, searcher, timeout)
        
    elif (element['Kind'] == 'Event'):
        readEvent(element, typ, searcher, timeout)
        
    elif (element['Kind'] == 'Constructor'):
        if (element['CoveragePercent'] > 0):
            # In json static ctors have the same name as regular ones
            name = element['Name']
            if (name in ctors):
                ctors[name] += 1
                name = f"{name}-{ctors[name]}"
            else:
                ctors[name] = 1
            method_rows.append([searcher, timeout, typ, name, element['CoveragePercent'], element['TotalStatements']])
    
    elif (element['Kind'] == 'Method'):
        if (element['CoveragePercent'] > 0):
            method_rows.append([searcher, timeout, typ, element['Name'], element['CoveragePercent'], element['TotalStatements']])
            
    elif (element['Kind'] == 'Assembly' or element['Kind'] == 'Root'):
        for child in element.get('Children', []):
            readElement(child, "", "", searcher, timeout)
            
    elif (element['Kind'] == 'Namespace'):
        if (element['CoveragePercent'] > 0):
            for child in element.get('Children', []):
                readElement(child, "", element['Name'], searcher, timeout)
            
    elif (element['Kind'] == 'Type'):
        type_rows.append([searcher, timeout, namespace, element['Name'], element['CoveragePercent'], element['TotalStatements']])
        if (element['CoveragePercent'] > 0):
            for child in element.get('Children', []):
                readElement(child, element['Name'], namespace, searcher, timeout)            
    else:
        print(f"Unknown kind: {element['Kind']}")
        return

def readJson(searcher, timeout):
    with open(f'{root_dir}/{searcher}/{timeout}/coverage.json', 'r') as file:
        # Remove some strange invisible symbols
        data = file.read()[3:]
        data = json.loads(data)
        readElement(data, "", "", searcher_dir.name, timeout_dir.name)
        ctors.clear()
    return method_rows, type_rows

searchers = []
timeouts = set()

for searcher_dir in filter(lambda f: f.is_dir(), os.scandir(root_dir)):
    searchers.append(searcher_dir.name)
    for timeout_dir in filter(lambda f: f.is_dir(), os.scandir(searcher_dir)):
        readJson(searcher_dir.name, timeout_dir.name)
        timeouts.add(timeout_dir.name)

method_df = pd.DataFrame(method_rows, columns = ['Searcher', 'Timeout', 'Type', 'Member', 'Coverage', 'Statements'])
type_df = pd.DataFrame(type_rows, columns = ['Searcher', 'Timeout', 'Namespace', 'Type', 'Coverage', 'Statements']) 

def drawCharts(df, charts_column, bars_column, group_column, element_column, colors):
    for chart, chart_group in df.groupby(charts_column):
        crosstabs = []
        ratios = []
        total_bars = 0

        grouped_by_type = chart_group.groupby(group_column)

        for typ, type_group in grouped_by_type:
            cross = pd.crosstab(type_group[element_column], type_group[bars_column], values=type_group['Coverage'], aggfunc='mean')
            cross = cross.reset_index()
            cross["Max"] = cross[cross.columns[1:]].max(axis=1)
            cross = cross.sort_values('Max')
            cross = cross.drop('Max', axis = 1)
            cross[element_column] = ['\n'.join(wrap(x, 20)) for x in cross[element_column]]
            cross = cross.set_index(element_column)
            crosstabs.append((typ, cross))
            ratios.append(len(cross))
            total_bars += len(cross)

        fig, axes = plt.subplots(len(ratios), 1, gridspec_kw={'height_ratios': ratios}, figsize=(6, total_bars), sharex=True)
        fig.suptitle(f"{charts_column}: {chart}", fontsize=12, y=1)
        plt.rcParams.update({'font.size': 7})

        for i, (typ, cross) in enumerate(crosstabs):
            y_axis = axes[i].get_yaxis()            
            y_axis.get_label().set_visible(False)
            axes[i].set_title(typ)
            g = cross.plot.barh(ax = axes[i], color = colors)
            for p in g.patches:
                width = p.get_width()
                height = p.get_height()
                x, y = p.get_xy()
                g.annotate(f'{width}%', (max(4, x + width / 2), y + height * 0.5), ha='center')

        plt.tight_layout()
        plt.savefig(f'{root_dir}/{chart}-{group_column}-{element_column}.png')

colors = ['darkorange', 'cornflowerblue', 'mediumseagreen', 'mediumpurple', 'red', 'gold']
drawCharts(method_df, 'Timeout', 'Searcher', 'Type', 'Member',  dict(zip(searchers, colors)))
drawCharts(type_df, 'Timeout', 'Searcher', 'Namespace', 'Type',  dict(zip(searchers, colors)))
drawCharts(method_df, 'Searcher', 'Timeout', 'Type', 'Member', dict(zip(list(timeouts), colors)))
drawCharts(type_df, 'Searcher', 'Timeout', 'Namespace', 'Type', dict(zip(list(timeouts), colors)))
