import glob, csv, numpy

fnames = glob.glob('Offline/*.csv')

with open('AX-CPT_individual_output_correctedSD.csv', 'w', newline = '\n') as o:
    writer = csv.writer(o)
    writer.writerow(['P_ID', 
                     'T',
                     'AX_Num_Trials', 
                     'BX_Num_Trials',
                     'AY_Num_Trials',
                     'BY_Num_Trials',
                     'AX_Avg_RT', 'BX_Avg_RT', 'AY_Avg_RT', 'BY_Avg_RT',
                     'AX_Correct_Avg_RT', 'AX_Incorrect_Avg_RT',
                     'BX_Correct_Avg_RT', 'BX_Incorrect_Avg_RT',
                     'AY_Correct_Avg_RT', 'AY_Incorrect_Avg_RT',
                     'BY_Correct_Avg_RT', 'BY_Incorrect_Avg_RT',
                     'AX_Error',
                     'BX_Error',
                     'AY_Error',
                     'BY_Error'])

    for fname in fnames:
        data = {'ID' : fname[9:12].strip('S').strip('_'),
                'T' : '',
                'AX_RTs': [], 'BX_RTs' : [], 'AY_RTs' : [], 'BY_RTs' : [],
                'AX_Correct_RTs' : [], 'AX_Incorrect_RTs' : [],
                'BX_Correct_RTs' : [], 'BX_Incorrect_RTs' : [],
                'AY_Correct_RTs' : [], 'AY_Incorrect_RTs' : [],
                'BY_Correct_RTs' : [], 'BY_Incorrect_RTs' : [],
                'AX_Correct' : 0, 'AX_Incorrect' : 0,
                'BX_Correct' : 0, 'BX_Incorrect' : 0,
                'AY_Correct' : 0, 'AY_Incorrect' : 0,
                'BY_Correct' : 0, 'BY_Incorrect' : 0}
            
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
                        
                        if row[7] == 'AX' and row[8] == 'Probe':
                            t0 = float(row[13])
                            t1 = float(next(reader)[13])
                            data['AX_RTs'].append(t1-t0)
                            if row[20] == 'hit':
                                data['AX_Correct'] += 1
                                data['AX_Correct_RTs'].append(t1-t0)
                            elif row[20] == 'incorrect':
                                data['AX_Incorrect'] += 1
                                data['AX_Incorrect_RTs'].append(t1-t0)
                                
                        if row[7] == 'BX' and row[8] == 'Probe':
                            t0 = float(row[13])
                            t1 = float(next(reader)[13])
                            data['BX_RTs'].append(t1-t0)
                            if row[20] == 'hit':
                                data['BX_Correct'] += 1
                                data['BX_Correct_RTs'].append(t1-t0)
                            elif row[20] == 'incorrect':
                                data['BX_Incorrect'] += 1
                                data['BX_Incorrect_RTs'].append(t1-t0)
                                
                        if row[7] == 'AY' and row[8] == 'Probe':
                            t0 = float(row[13])
                            t1 = float(next(reader)[13])
                            data['AY_RTs'].append(t1-t0)
                            if row[20] == 'hit':
                                data['AY_Correct'] += 1
                                data['AY_Correct_RTs'].append(t1-t0)
                            elif row[20] == 'incorrect':
                                data['AY_Incorrect'] += 1
                                data['AY_Incorrect_RTs'].append(t1-t0)
                                
                        if row[7] == 'BY' and row[8] == 'Probe':
                            t0 = float(row[13])
                            t1 = float(next(reader)[13])
                            data['BY_RTs'].append(t1-t0)
                            if row[20] == 'hit':
                                data['BY_Correct'] += 1
                                data['BY_Correct_RTs'].append(t1-t0)
                            elif row[20] == 'incorrect':
                                data['BY_Incorrect'] += 1
                                data['BY_Incorrect_RTs'].append(t1-t0)
            
            r = data['AX_Correct_RTs']
            data['AX_Correct_RTs'] = []
            sd_above = numpy.average(r) + (numpy.std(r)*2)
            sd_below = numpy.average(r) - (numpy.std(r)*2)
            for v in r:
                if v >= sd_below and v <= sd_above:
                    data['AX_Correct_RTs'].append(v)

            
            k = data['AY_Correct_RTs']
            data['AY_Correct_RTs'] = []
            sd_above = numpy.average(k) + (numpy.std(k)*2)
            sd_below = numpy.average(k) - (numpy.std(k)*2)
            for l in k:
                if l >= sd_below and l <= sd_above:
                    data['AY_Correct_RTs'].append(l)
            
            e = data['BX_Correct_RTs']
            data['BX_Correct_RTs'] = []
            sd_above = numpy.average(e) + (numpy.std(e)*2)
            sd_below = numpy.average(e) - (numpy.std(e)*2)
            for w in e:
                if w >= sd_below and w <= sd_above:
                    data['BX_Correct_RTs'].append(w)

            
            p = data['BY_Correct_RTs']
            data['BY_Correct_RTs'] = []
            sd_above = numpy.average(p) + (numpy.std(p)*2)
            sd_below = numpy.average(p) - (numpy.std(p)*2)
            for c in p:
                if c >= sd_below and c <= sd_above:
                    data['BY_Correct_RTs'].append(c)
            
            a = data['AX_RTs']
            data['AX_RTs'] = []
            sd_above = numpy.average(a) + (numpy.std(a)*2)
            sd_below = numpy.average(a) - (numpy.std(a)*2)
            for b in a:
                if b >= sd_below and b <= sd_above:
                    data['AX_RTs'].append(b)

            
            d = data['AY_RTs']
            data['AY_RTs'] = []
            sd_above = numpy.average(d) + (numpy.std(d)*2)
            sd_below = numpy.average(d) - (numpy.std(d)*2)
            for g in d:
                if g >= sd_below and g <= sd_above:
                    data['AY_RTs'].append(g)
            
            u = data['BX_RTs']
            data['BX_RTs'] = []
            sd_above = numpy.average(u) + (numpy.std(u)*2)
            sd_below = numpy.average(u) - (numpy.std(u)*2)
            for z in u:
                if z >= sd_below and z <= sd_above:
                    data['BX_RTs'].append(z)

            
            q = data['BY_RTs']
            data['BY_RTs'] = []
            sd_above = numpy.average(q) + (numpy.std(q)*2)
            sd_below = numpy.average(q) - (numpy.std(q)*2)
            for s in q:
                if s >= sd_below and s <= sd_above:
                    data['BY_RTs'].append(s)
            

                                
            try: ax_error = data['AX_Incorrect']/(data['AX_Incorrect']+data['AX_Correct']) 
            except ZeroDivisionError: ax_error = 'error'; pass
            try: bx_error = data['BX_Incorrect']/(data['BX_Incorrect']+data['BX_Correct']) 
            except ZeroDivisionError: bx_error = 'error'; pass
            try: ay_error = data['AY_Incorrect']/(data['AY_Incorrect']+data['AY_Correct']) 
            except ZeroDivisionError: ay_error = 'error'; pass
            try: by_error = data['BY_Incorrect']/(data['BY_Incorrect']+data['BY_Correct']) 
            except ZeroDivisionError: by_error = 'error'; pass                      
            writer.writerow([data['ID'],
                             data['T'],
                             data['AX_Correct'] + data['AX_Incorrect'],
                             data['BX_Correct'] + data['BX_Incorrect'],
                             data['AY_Correct'] + data['AY_Incorrect'],
                             data['BY_Correct'] + data['BY_Incorrect'], 
                             (numpy.average(data['AX_RTs']))/10,
                             (numpy.average(data['BX_RTs']))/10, 
                             (numpy.average(data['AY_RTs']))/10,
                             (numpy.average(data['BY_RTs']))/10,
                             (numpy.average(data['AX_Correct_RTs']))/10, (numpy.average(data['AX_Incorrect_RTs']))/10,
                             (numpy.average(data['BX_Correct_RTs']))/10, (numpy.average(data['BX_Incorrect_RTs']))/10,
                             (numpy.average(data['AY_Correct_RTs']))/10, (numpy.average(data['AY_Incorrect_RTs']))/10,
                             (numpy.average(data['BY_Correct_RTs']))/10, (numpy.average(data['BY_Incorrect_RTs']))/10,
                             ax_error,
                             bx_error,
                             ay_error,
                             by_error])
        f.close()
o.close()               