import numpy, csv, glob, os

fnames = sorted(glob.glob('csv/SSRT*.csv'),key=os.path.basename)

outfilename = 'all_SSRT_output.csv'

with open(outfilename, 'w', newline = '\n') as o:
    writer = csv.writer(o)
    writer.writerow(['ID', 
                     'Session',
                     'Correct Go Trials',
                     'Correct Inhibit Trials',
                     'Mean Go_Corr RT',
                     'Mean Stop_Incorrect RT', 
                     'p',
                     'n',
                     'nth_rt',
                     'mean_ssd',                                        
                     'SSRT',
                     'Std. DV'])

    for fname in fnames:
        if fname != outfilename:
            data = {'ID' : fname,
                    'T' : '',
                    'go_rts' : [],
                    'num_inhibit' : 0.0,
                    'num_correct_inhibit' : 0.0,
                    'p' : 0.0,
                    'nth_rt' : 0.0,
                    'ssd' : [],
                    'mean_ssd' : 0.0,
                    'SSRT' : 0.0,
                    'incorr_stop_rts' : []}
             
            print (f'fname={fname}')
            
            if 'S0' in fname:
                data['T'] = '0'
            elif 'S1' in fname:
                data['T'] = '1'
            elif 'S2' in fname:
                data['T'] = '2'
            
            with open(fname, newline = '') as f:
                reader = csv.reader(f)
                idx=0
                for row in reader:
                    idx += 1
                    if idx >= 11:
                        if float(row[6]) == 0 and float(row[8]) >= 100 and float(row[8]) <= 5000:
                            if float(row[5]) == 2 and float(row[7]) == 1:
                                data['go_rts'].append(float(row[8]))
                            if float(row[5]) == 1 and float(row[7]) == 2:
                                data['go_rts'].append(float(row[8]))
                        
                        elif float(row[6]) != 0:
                            data['num_inhibit'] += 1
                            data['ssd'].append(float(row[6]))
                            if float(row[8]) == 0:
                                data['num_correct_inhibit'] += 1
                            elif float(row[6]) != 0:
                                data['incorr_stop_rts'].append(float(row[6]))
            
            
                if len(data['go_rts']) > 0 and data['num_inhibit'] > 0 and data['num_correct_inhibit'] > 0:                   
                    r = data['go_rts']
                    data['go_rts'] = []
                    sd_above = numpy.average(r) + (numpy.std(r)*2)
                    sd_below = numpy.average(r) - (numpy.std(r)*2)
                    for v in r:
                        if v >= sd_below and v <= sd_above:
                            data['go_rts'].append(v)
                    
                    x = data['incorr_stop_rts']
                    data['incorr_stop_rts'] = []
                    sd_above = numpy.average(x) + (numpy.std(x)*2)
                    sd_below = numpy.average(x) - (numpy.std(x)*2)
                    for k in x:
                        if k >= sd_below and k <= sd_above:
                            data['incorr_stop_rts'].append(k)


                    
                    data['mean_go_rt'] = numpy.average(data['go_rts'])
                    data['Std. DV'] = numpy.std(data['go_rts'], ddof = 1)
                    data['p'] = 1-(data['num_correct_inhibit'] / data['num_inhibit'])
                    data['n'] = int((data['p']) * len(data['go_rts']))
                    data['go_rts'].sort()
                    data['nth_rt'] = data['go_rts'][data['n']-1]
                    data['mean_ssd'] = numpy.average(data['ssd'])
                    data['SSRT'] = data['nth_rt'] - data['mean_ssd']
                    
                    writer.writerow([data['ID'],
                                    data['T'],
                                    len(data['go_rts']),  
                                    data['num_correct_inhibit'],
                                    data['mean_go_rt'],
                                    numpy.average(data['incorr_stop_rts']),
                                    data['p'],
                                    data['n'],
                                    data['nth_rt'],
                                    data['mean_ssd'],
                                    data['SSRT'],
                                    data['Std. DV']])
                else:
                    writer.writerow([data['ID'],
                                     data['T'],
                                     len(data['go_rts']),  
                                     data['num_correct_inhibit']])
            f.close()
o.close()
print ("Done")