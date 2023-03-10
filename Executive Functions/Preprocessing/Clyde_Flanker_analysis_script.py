import glob, csv, numpy

fnames = glob.glob('Clyde/*.csv')

with open('Clyde_Flanker_individual_output_correctedSD.csv', 'w', newline = '') as o:
    writer = csv.writer(o)
    writer.writerow(['P_ID', 
                     'T',
                     'Switch_RT',
                     'Stay_RT',
                     'Switch_RT_corr',
                     'Stay_RT_corr',
                     'Accuracy_Stay',
                     'Accuracy_Switch'
                     ])

    for fname in fnames:
        if fname != 'Chloe_Flanker_individual_output.csv':
            data = {'ID' : fname,
                    'T' : 0,
                    'Accuracy_Stay': 0,
                    'Accuracy_Switch': 0,
                    'Switch_RT': [],
                    'Stay_RT': [],
                    'Switch_RT_corr': [],
                    'Stay_RT_corr': [],
                    'Org Trial': 0}
                
            #if 'S0' in fname:
            #   data['T'] = '0'
            #elif 'S1' in fname:
            #    data['T'] = '1'
            #elif 'S2' in fname:
            #    data['T'] = '2'
            
            with open(fname, newline = '') as f:
                reader = csv.reader(f)
                            
                for row in reader:
                    
                    if len(row) > 4:
                        
                        if row[4] == 'Main':
                            data['Org Trial'] += 1
                            if data['Org Trial'] == 4 or data['Org Trial'] == 6 or data['Org Trial'] == 9 or data['Org Trial'] == 13 or data['Org Trial'] == 16 or data['Org Trial'] == 19 or data['Org Trial'] == 21 or data['Org Trial'] == 23 or data['Org Trial'] == 27 or data['Org Trial'] == 30 or data['Org Trial'] == 34 or data['Org Trial'] == 37:
                                t0 = float(row[14])
                                t1 = float(next(reader)[14])
                                data['Switch_RT'].append(t1-t0)
                                if row[21] == 'hit':
                                    data['Accuracy_Switch'] += 1
                                    t0 = float(row[14])
                                    t1 = float(next(reader)[14])
                                    data['Switch_RT_corr'].append(t1-t0)

                            if data['Org Trial'] == 1 or data['Org Trial'] == 2 or data['Org Trial'] == 3 or data['Org Trial'] == 5 or data['Org Trial'] == 7 or data['Org Trial'] == 8 or data['Org Trial'] == 10 or data['Org Trial'] == 11 or data['Org Trial'] == 12 or data['Org Trial'] == 14 or data['Org Trial'] == 15 or data['Org Trial'] == 17 or data['Org Trial'] == 18 or data['Org Trial'] == 20 or data['Org Trial'] == 22 or data['Org Trial'] == 24 or data['Org Trial'] == 25 or data['Org Trial'] == 26 or data['Org Trial'] == 28 or data['Org Trial'] == 29 or  data['Org Trial'] ==31 or data['Org Trial'] == 32 or data['Org Trial'] == 33 or data['Org Trial'] == 35 or data['Org Trial'] == 36 or data['Org Trial'] == 38 or data['Org Trial'] == 39 or data['Org Trial'] == 40:
                                t0 = float(row[14])
                                t1 = float(next(reader)[14])
                                data['Stay_RT'].append(t1-t0)
                                if row[21] == 'hit':
                                    data['Accuracy_Stay'] += 1
                                    t0 = float(row[14])
                                    t1 = float(next(reader)[14])
                                    data['Stay_RT_corr'].append(t1-t0)

                try: stay_error = data['Accuracy_Stay']/(len(data['Stay_RT']))
                except ZeroDivisionError: stay_error = 'error'; pass
                try: switch_error = data['Accuracy_Switch']/(len(data['Switch_RT']))
                except ZeroDivisionError: switch_error = 'error'; pass
               
                p = data['Switch_RT']
                data['Switch_RT'] = []
                sd_above = numpy.average(p) + (numpy.std(p)*2)
                sd_below = numpy.average(p) - (numpy.std(p)*2)
                for c in p:
                    if c >= sd_below and c <= sd_above:
                        data['Switch_RT'].append(c)
            
                a = data['Stay_RT']
                data['Stay_RT'] = []
                sd_above = numpy.average(a) + (numpy.std(a)*2)
                sd_below = numpy.average(a) - (numpy.std(a)*2)
                for b in a:
                    if b >= sd_below and b <= sd_above:
                        data['Stay_RT'].append(b)
                
                w = data['Switch_RT_corr']
                data['Switch_RT_corr'] = []
                sd_above = numpy.average(w) + (numpy.std(w)*2)
                sd_below = numpy.average(w) - (numpy.std(w)*2)
                for x in w:
                    if x >= sd_below and x <= sd_above:
                        data['Switch_RT_corr'].append(x)
            
                k = data['Stay_RT_corr']
                data['Stay_RT_corr'] = []
                sd_above = numpy.average(k) + (numpy.std(k)*2)
                sd_below = numpy.average(k) - (numpy.std(k)*2)
                for l in k:
                    if l >= sd_below and l <= sd_above:
                        data['Stay_RT_corr'].append(k)


                writer.writerow([data['ID'], data['T'],
                                (numpy.median(data['Switch_RT']))/10,
                                (numpy.median(data['Stay_RT']))/10,
                                (numpy.median(data['Switch_RT_corr']))/10,
                                (numpy.median(data['Stay_RT_corr']))/10,
                                stay_error,
                                switch_error])
                                 
    f.close()
o.close()               