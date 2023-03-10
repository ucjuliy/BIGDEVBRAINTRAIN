import glob, csv, numpy

fnames = glob.glob('*.csv')

outfile = 'Animalstroop_individual_output.csv'

with open(outfile, 'w', newline = '\n') as o:
    writer = csv.writer(o)
    writer.writerow(['P_ID', 
                     'T',
                     'Neutral_Correct',
                     'Neutral_Incorrect',
                     'Neutral_error',
                     'Incongruent_Correct',
                     'Incongruent_Incorrect',
                     'Incongruent_error',
                     'Neutral_Avg_RT',
                     'Incongruent_Avg_RT',
                     'Neutral_Correct_Avg_RT',
                     'Neutral_Incorrect_Avg_RT',
                     'Incongruent_Correct_Avg_RT',
                     'Incongruent_Incorrect_Avg_RT'])

    for fname in fnames:
        

            data = {'ID' : '',
                    'T' : '',
                    'Neutral_Correct' : 0,
                    'Neutral_Incorrect' : 0,
                    'Incongruent_Correct' : 0, 
                    'Incongruent_Incorrect' : 0,
                    'Neutral_Correct_RTs' : [],
                    'Neutral_Incorrect_RTs' : [],
                    'Incongruent_Correct_RTs' : [], 
                    'Incongruent_Incorrect_RTs' : [],
                    'Neutral_Avg_RT' : [],
                    'Incongruent_Avg_RT': []}
            
            print (f'fname={fname}')

            #Extract the number from 3rd part of file name
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
                           # print ( 'Row', row )
                            
                            data['ID'] = row[14]
                            
                            if row[7] == 'Neutral':
                                if row[12] == 'Correct':
                                    data['Neutral_Correct'] += 1
                                    t0 = float(row[13])
                                    #t1 = float(next(reader)[13])
                                    data['Neutral_Correct_RTs'].append(t0)
                                elif row[12] == 'Incorrect':
                                    data['Neutral_Incorrect'] += 1
                                    t0 = float(row[13])
                                    #t1 = float(next(reader)[13])
                                    data['Neutral_Incorrect_RTs'].append(t0)
                                    
                            if row[7] == 'Incongruent':
                                if row[12] == 'Correct':
                                    data['Incongruent_Correct'] += 1
                                    t0 = float(row[13])
                                    #t1 = float(next(reader)[13])
                                    data['Incongruent_Correct_RTs'].append(t0)
                                elif row[12] == 'Incorrect':
                                    data['Incongruent_Incorrect'] += 1
                                    t0 = float(row[13])
                                    #t1 = float(next(reader)[13])
                                    data['Incongruent_Incorrect_RTs'].append(t0)
            
                                    
                try: Neutral_error = data['Neutral_Incorrect']/(data['Neutral_Incorrect']+data['Neutral_Correct'])
                except ZeroDivisionError: Neutral_error = 'error'; pass
                try: Incongruent_error = data['Incongruent_Incorrect']/(data['Incongruent_Incorrect']+data['Incongruent_Correct'])
                except ZeroDivisionError: Incongruent_error = 'error'; pass

                data['Neutral_Avg_RT'] = data['Neutral_Correct_RTs']+ data['Neutral_Incorrect_RTs']
                data['Incongruent_Avg_RT'] = data['Incongruent_Correct_RTs']+ data['Incongruent_Incorrect_RTs']

                p = data['Neutral_Avg_RT']
                data['Neutral_Avg_RT'] = []
                sd_above = numpy.average(p) + (numpy.std(p)*2)
                sd_below = numpy.average(p) - (numpy.std(p)*2)
                for c in p:
                    if c >= sd_below and c <= sd_above:
                        data['Neutral_Avg_RT'].append(c)
            
                a = data['Incongruent_Avg_RT']
                data['Incongruent_Avg_RT'] = []
                sd_above = numpy.average(a) + (numpy.std(a)*2)
                sd_below = numpy.average(a) - (numpy.std(a)*2)
                for b in a:
                    if b >= sd_below and b <= sd_above:
                        data['Incongruent_Avg_RT'].append(b)

            
                d = data['Incongruent_Correct_RTs']
                data['Incongruent_Correct_RTs'] = []
                sd_above = numpy.average(d) + (numpy.std(d)*2)
                sd_below = numpy.average(d) - (numpy.std(d)*2)
                for g in d:
                    if g >= sd_below and g <= sd_above:
                        data['Incongruent_Correct_RTs'].append(g)
            
                u = data['Neutral_Correct_RTs']
                data['Neutral_Correct_RTs'] = []
                sd_above = numpy.average(u) + (numpy.std(u)*2)
                sd_below = numpy.average(u) - (numpy.std(u)*2)
                for z in u:
                    if z >= sd_below and z <= sd_above:
                        data['Neutral_Correct_RTs'].append(z)


                writer.writerow([data['ID'],
                                 data['T'],
                                 data['Neutral_Correct'],
                                 data['Neutral_Incorrect'],
                                 Neutral_error,
                                 data['Incongruent_Correct'],
                                 data['Incongruent_Incorrect'],
                                 Incongruent_error,
                                 numpy.average(data['Neutral_Avg_RT']),
                                 numpy.average(data['Incongruent_Avg_RT']),
                                 numpy.average(data['Neutral_Correct_RTs']),
                                 numpy.average(data['Neutral_Incorrect_RTs']),
                                 numpy.average(data['Incongruent_Correct_RTs']),
                                 numpy.average(data['Incongruent_Incorrect_RTs'])])
    f.close()
o.close()               