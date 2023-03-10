import glob, csv, numpy

fnames = glob.glob('Chloe/files/*.csv')

with open('Chloe_Flanker_individual_output_correctedSD.csv', 'w', newline = '\n') as o:
    writer = csv.writer(o)
    writer.writerow(['P_ID', 
                     'T',
                     'Congruent_Correct', 
                     'Congruent_Incorrect',
                     'Congruent_error',
                     'Incongruent_Correct',
                     'Incongruent_Incorrect',
                     'Incongruent_error',
                     'Congruent_Correct_Avg_RT', 
                     'Congruent_Incorrect_Avg_RT',
                     'Incongruent_Correct_Avg_RT',
                     'Incongruent_Incorrect_Avg_RT',
                     'Congruent_RT', 
                     'Incongruent_RT'])

    for fname in fnames:
        if fname != 'Chloe_Flanker_individual_output.csv':
            data = {'ID' : '',
                    'T' : '',
                    'Congruent_Correct' : 0, 
                    'Congruent_Incorrect' : 0,
                    'Incongruent_Correct' : 0, 
                    'Incongruent_Incorrect' : 0,
                    'Congruent_Correct_RTs' : [], 
                    'Congruent_Incorrect_RTs' : [],
                    'Incongruent_Correct_RTs' : [], 
                    'Incongruent_Incorrect_RTs' : [],
                    'Congruent_RT' : [],
                    'Incongruent_RT' : []}
                
            if 'S0' in fname:
                data['T'] = '0'
            elif 'S1' in fname:
                data['T'] = '1'
            elif 'S2' in fname:
                data['T'] = '2'
            
            with open(fname, newline = '') as f:
                reader = csv.reader(f)
                            
                for row in reader:
                    
                    if len(row) > 4:
                        
                        if row[4] == 'Main':
                            
                            data['ID'] = row[13]
                            
                            if row[12] == 'Compatible':
                                t0 = float(row[14])
                                t1 = float(next(reader)[14])
                                data['Congruent_RT'].append(t1-t0)
                                if row[21] == 'hit':
                                    data['Congruent_Correct'] += 1
                                    t0 = float(row[14])
                                    t1 = float(next(reader)[14])
                                    data['Congruent_Correct_RTs'].append(t1-t0)
                                elif row[21] == 'incorrect':
                                    data['Congruent_Incorrect'] += 1
                                    t0 = float(row[14])
                                    t1 = float(next(reader)[14])
                                    data['Congruent_Incorrect_RTs'].append(t1-t0)
                                    
                            if row[12] == 'Incompatible':
                                t0 = float(row[14])
                                t1 = float(next(reader)[14])
                                data['Incongruent_RT'].append(t1-t0)
                                if row[21] == 'hit':
                                    data['Incongruent_Correct'] += 1
                                    t0 = float(row[14])
                                    t1 = float(next(reader)[14])
                                    data['Incongruent_Correct_RTs'].append(t1-t0)
                                elif row[21] == 'incorrect':
                                    data['Incongruent_Incorrect'] += 1
                                    t0 = float(row[14])
                                    t1 = float(next(reader)[14])
                                    data['Incongruent_Incorrect_RTs'].append(t1-t0)
                                    
                try: congruent_error = data['Congruent_Incorrect']/(data['Congruent_Incorrect']+data['Congruent_Correct']) 
                except ZeroDivisionError: congruent_error = 'error'; pass
                try: incongruent_error = data['Incongruent_Incorrect']/(data['Incongruent_Incorrect']+data['Incongruent_Correct']) 
                except ZeroDivisionError: incongruent_error = 'error'; pass

                #Congruent_RT = (data ['Congruent_Incorrect_RTs'] + ['Congruent_Correct_RTs'])
                #Incongruent_RT = (data ['Incongruent_Incorrect_RTs'] + ['Incongruent_Correct_RTs'])
                p = data['Congruent_Correct_RTs']
                data['Congruent_Correct_RTs'] = []
                sd_above = numpy.average(p) + (numpy.std(p)*2)
                sd_below = numpy.average(p) - (numpy.std(p)*2)
                for c in p:
                    if c >= sd_below and c <= sd_above:
                        data['Congruent_Correct_RTs'].append(c)
            
                a = data['Incongruent_Correct_RTs']
                data['Incongruent_Correct_RTs'] = []
                sd_above = numpy.average(a) + (numpy.std(a)*2)
                sd_below = numpy.average(a) - (numpy.std(a)*2)
                for b in a:
                    if b >= sd_below and b <= sd_above:
                        data['Incongruent_Correct_RTs'].append(b)
                
                w = data['Congruent_RT']
                data['Congruent_RT'] = []
                sd_above = numpy.average(w) + (numpy.std(w)*2)
                sd_below = numpy.average(w) - (numpy.std(w)*2)
                for x in w:
                    if x >= sd_below and x <= sd_above:
                        data['Congruent_RT'].append(x)
            
                k = data['Incongruent_RT']
                data['Incongruent_RT'] = []
                sd_above = numpy.average(k) + (numpy.std(k)*2)
                sd_below = numpy.average(k) - (numpy.std(k)*2)
                for l in k:
                    if l >= sd_below and l <= sd_above:
                        data['Incongruent_RT'].append(k)

            
                writer.writerow([data['ID'],
                                 data['T'],
                                 data['Congruent_Correct'],
                                 data['Congruent_Incorrect'],
                                 congruent_error,
                                 data['Incongruent_Correct'],
                                 data['Incongruent_Incorrect'],
                                 incongruent_error,
                                 (numpy.median(data['Congruent_Correct_RTs']))/10,
                                 (numpy.median(data['Congruent_Incorrect_RTs']))/10,
                                 (numpy.median(data['Incongruent_Correct_RTs']))/10,
                                 (numpy.median(data['Incongruent_Incorrect_RTs']))/10,
                                 (numpy.median(data['Congruent_RT']))/10,
                                 (numpy.median(data['Incongruent_RT']))/10])
        f.close()
o.close()               
