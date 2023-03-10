import glob, csv, numpy

fnames = glob.glob('*.csv')

out = 'N-Back_individual_output_correctedSD.csv'

with open(out, 'w', newline = '\n') as o:
    writer = csv.writer(o)
    writer.writerow(['P_ID', 
                     'T',
                     '1Back_Num_Trials',
                     '1Back_Num_Hits',
                     '1Back_Num_Correct_Rejections',
                     '1Back_Num_FalseAlarms',
                     '1Back_Num_Misses',
                     '1Back_Hits_av_RT',
                     '1Back_FalseAlarms_av_RT',
                     '2Back_Num_Trials',
                     '2Back_Num_Hits',
                     '2Back_Num_Correct_Rejections',
                     '2Back_Num_FalseAlarms',
                     '2Back_Num_Misses',
                     '2Back_Hits_av_RT',
                     '2Back_FalseAlarms_av_RT'])

    for fname in fnames:
        data = {'ID' : fname[8:11].strip('S').strip('_'),
                'T' : '',
                '1Back_num_trials' : 0,
                '1Back_num_hits' : 0,
                '1Back_num_falsealarms' : 0,
                '1Back_num_misses' : 0,
                '1Back_num_correct_rejections' : 0,
                '1Back_hits_rts' : [],
                '1Back_FAs_rts' : [],
                '2Back_num_trials' : 0,
                '2Back_num_hits' : 0,
                '2Back_num_falsealarms' : 0,
                '2Back_num_misses' : 0,
                '2Back_num_correct_rejections' : 0,
                '2Back_hits_rts' : [],
                '2Back_FAs_rts' : []}
                
          
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
                    
                    if row[4] == '1':
                        data['1Back_num_trials'] += 1
                        if row[11] == 'Hit':
                            data['1Back_num_hits'] += 1
                            t0 = float(row[13])
                            t1 = float(next(reader)[13])
                            data['1Back_hits_rts'].append(t1-t0)
                        if row[11] == 'False Alarm':
                            data['1Back_num_falsealarms'] += 1
                            t0 = float(row[13])
                            t1 = float(next(reader)[13])
                            data['1Back_FAs_rts'].append(t1-t0)
                            
                        if row[11] == 'Miss':
                            data['1Back_num_misses'] += 1
                        if row[11] == 'Correct Rejection':
                            data['1Back_num_correct_rejections'] += 1
                            
                            
                    if row[4] == '2':
                        data['2Back_num_trials'] += 1
                        if row[11] == 'Hit':
                            data['2Back_num_hits'] += 1
                            t0 = float(row[13])
                            t1 = float(next(reader)[13])
                            data['2Back_hits_rts'].append(t1-t0)
                        if row[11] == 'False Alarm':
                            data['2Back_num_falsealarms'] += 1
                            t0 = float(row[13])
                            t1 = float(next(reader)[13])
                            data['2Back_FAs_rts'].append(t1-t0)
                            
                        if row[11] == 'Miss':
                            data['2Back_num_misses'] += 1
                        if row[11] == 'Correct Rejection':
                            data['2Back_num_correct_rejections'] += 1   

                p = data['1Back_hits_rts']
                data['1Back_hits_rts'] = []
                sd_above = numpy.average(p) + (numpy.std(p)*2)
                sd_below = numpy.average(p) - (numpy.std(p)*2)
                for c in p:
                    if c >= sd_below and c <= sd_above:
                        data['1Back_hits_rts'].append(c)
            
                a = data['2Back_hits_rts']
                data['2Back_hits_rts'] = []
                sd_above = numpy.average(a) + (numpy.std(a)*2)
                sd_below = numpy.average(a) - (numpy.std(a)*2)
                for b in a:
                    if b >= sd_below and b <= sd_above:
                        data['2Back_hits_rts'].append(b)

            writer.writerow([data['ID'],
                             data['T'],
                             data['1Back_num_trials'],
                             data['1Back_num_hits'],
                             data['1Back_num_correct_rejections'],
                             data['1Back_num_falsealarms'],
                             data['1Back_num_misses'],
                             numpy.average(data['1Back_hits_rts']),
                             numpy.average(data['1Back_FAs_rts']),
                             data['2Back_num_trials'],
                             data['2Back_num_hits'],
                             data['2Back_num_correct_rejections'],
                             data['2Back_num_falsealarms'],
                             data['2Back_num_misses'],
                             numpy.average(data['2Back_hits_rts']),
                             numpy.average(data['2Back_FAs_rts'])])
    f.close()
o.close()               