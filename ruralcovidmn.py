
#Determine rural vs urban death rates per 100,000 3/1-3/7/2020 to 
#https://www.health.state.mn.us/diseases/coronavirus/stats/death.html
#"Deaths by county of residence in Minnesota" <-confirmed COVID deaths only
#dcounty Sunday through Saturday:
#   5/3-5/9/2020 week corresponds to COVID 2020-05-01 week

f = open('dcounty.csv')
countyfile = f.readlines()
f.close()

countyweeks = []

for row in countyfile: #convert strings to list items
    countyweek = row.strip('\n')
    countyweek = countyweek.split(',')
    countyweeks.append(countyweek)

countyweeks = countyweeks[1::] #Remove headers

for row in countyweeks: #convert numbers to floats
    for i in range(len(row)):
        if i == 2 or i == 5 or i == 7:
            row[i] = float(row[i])

#Look up rural-urban definition, per CBSA
f = open('cbsa2fipsxw.csv')
cbsafile = f.readlines() #cbsafile is list
f.close()

cbsadict = {} #'XX County': 'Metropolitan Statistical Area' <-if not included, then rural

for row in cbsafile:
    cbsadata = row.strip('\n')
    cbsadata = cbsadata.split(',')
    cbsadict[cbsadata[4]] = cbsadata[2]

#Add variable "Rural" or "Non-Rural" to countyweeks
for row in countyweeks:
    if row[1] in cbsadict:
        row.append('Non-Rural')
    else:
        row.append('Rural')

#Determine rural and urban death rates per 100,000 per week

#dcounty.csv file -> match dcounty[x][3] to countyweeks[x][3]
#for each county, pull population [x][5] and deaths[x][7] and add to rural or urban based on [x][8]
#multiply by 100k to get per 100k rate
ruraldeaths = 0
ruralpop = 0

urbandeaths = 0
urbanpop = 0

week = countyweeks[0][3]

starcherdataset = []
starcherdataset.append(['week', 'ruralpop', 'ruraldeaths', 'ruraldeathrate', 'urbanpop', 'urbandeaths', 'urbandeathrate', 'excessdeathsYN', 'excessdeath%', 'ruralicubedsavg', 'urbanicubedsavg', 'ruralicuoccupancy', 'urbanicuoccupancy', 'ruralinptbeds', 'urbaninptbeds'])

def ruralcheck(row):
    if row[8] == 'Rural':
        return True
    elif row[8] == 'Non-Rural':
        return False
    return 'Error'

for row in countyweeks:
    if week == row[3]:
        if ruralcheck(row) == True:
            ruralpop += row[5]
            ruraldeaths += row[2]
        elif ruralcheck(row) == False:
            urbanpop += row[5]
            urbandeaths += row[2]
    else:
        starcherdataset.append([week, ruralpop, ruraldeaths, ruraldeaths/ruralpop*100000, urbanpop, urbandeaths, urbandeaths/urbanpop*100000])
        week = row[3]
        ruralpop = 0
        urbanpop = 0
        ruraldeaths = 0
        urbandeaths = 0
        if ruralcheck(row) == True:
            ruralpop += row[5]
            ruraldeaths += row[2]
        elif ruralcheck(row) == False:
            urbanpop += row[5]
            urbandeaths += row[2]

#Addition of excess deaths to dataset
#https://data.cdc.gov/NCHS/Excess-Deaths-Associated-with-COVID-19/xkkf-xrst/
    #starcherday -1 = excessdeath date

f = open('Excess_Deaths_Associated_with_COVID-19.csv')
excessdfile = f.readlines()
f.close()

mnexcess = []

for row in excessdfile: #convert strings to list items & ientify appropriate records
    mnweek = row.strip('\n')
    mnweek = mnweek.split(',')
    if mnweek[1] == 'Minnesota' and mnweek[10] == 'Predicted (weighted)' and mnweek[11] == 'All causes':
        mnexcess.append(mnweek)

for row in starcherdataset:
    if row[0] != 'week':
        date = row[0].split('/')  #[month, date, year]
        row[0] = date
        row[0][1] = int(row[0][1])
        row[0][0] = int(row[0][0])
        row[0][2] = int(row[0][2])

for row in mnexcess:
    date = row[0].split('-') #[year, month, date]
    row[0] = date

for row in mnexcess: #[year, month, date]
    year = int(row[0][0])
    month = int(row[0][1])
    date = int(row[0][2])+1
    for line in starcherdataset: #[month, date, year]
        if line[0][2] == year and line[0][0] == month and line[0][1] == date:
            #append to line: Exceedsthreshold[4] == True: return percent excess estimate[8]
            line.append(row[4])
            line.append(float(row[8]))
        elif line[0][2] == year and line[0][0] == (month+1) and line[0][1] == 1 and len(line)<9:
            line.append(row[4])
            line.append(float(row[8]))
        elif month == 12 and date == 32 and line[0][0] == 1 and line[0][1] == 1:
            line.append(row[4])
            line.append(float(row[8]))



#Determine rural and urban staffed ICU beds per [hospital size?] per week
mnhospitaldata = []
mnhospitaldata2 = []

fp = open('MN_Adult_COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_Facility.csv')
txt = fp.readlines()
fp.close()

for record in txt: #convert strings to list items & ientify appropriate records
    hosprecord = record.strip('\n')
    hosprecord = hosprecord.split(',')
    if hosprecord[8] == 'Critical Access Hospitals' or hosprecord[8] == 'Short Term': #Limit data by hospital type: 'Critical Access Hospital' and 'Short term' [8]
        mnhospitaldata.append(hosprecord)

print('Sorting file...')
def datesort(data):
    date = data[1]
    datelist = date.split('/')
    datestring = datelist[2]+datelist[0]+datelist[1]
    datestring = int(datestring)
    return datestring

mnhospitaldata.sort(key=datesort)

for row in mnhospitaldata:
    if row[1] != 'collection_week':
        date = row[1].split('/')  #[month, date, year]
        row[1] = date
        row[1][1] = int(row[1][1]) #date
        row[1][0] = int(row[1][0]) #month
        row[1][2] = int(row[1][2]) #year
    conv = lambda i : i or None
    rw = [conv(i) for i in row] #turn blank strings into None
    mnhospitaldata2.append(rw)

print('Imputing file...')
for row in mnhospitaldata2:
    for i in range(len(row)):
        for i in range(11, 70):
            if isinstance(row[i], str):
                row[i] = float(row[i])
            if row[i] == -999999: #Impute 2.5 (median of 1-4) for -999,999
                row[i] = 2.5

#total_staffed_adult_icu_beds_7_day_avg [21] 88.7% reported
#staffed_adult_icu_bed_occupancy_7_day_avg [23] 88.7% reported
#inpatient_beds_7_day_avg [19] 99.6% reported
ruralicubedsavg = 0
ruralicuoccupancy = 0
ruralinptbeds = 0

urbanicubedsavg = 0
urbanicuoccupancy = 0
urbaninptbeds = 0

def validappend(date, data): #[month, date, year] from MN Hosp Dataset, value to append
    for line in starcherdataset:
        if date[0] == line[0][0] and date[1]+2 == line[0][1] and date[2] == line[0][2]:
            line.append(data)
        elif line[0][1] == 1 and date[1]+2 >= 30 and date[0]+1 == line[0][0] and date[2] == line[0][2]:
            line.append(data)
        elif line[0][1] == 2 and date[1]+2 >= 30 and date[0]+1 == line[0][0] and date[2] == line[0][2]:
            line.append(data)
        elif line[0][0] == 1 and line[0][1] == 1 and date[0] == 12 and date[1]==30 and date[2]+1 == line[0][2]:
            line.append(data)
        elif line[0][0] == 1 and line[0][1] == 2 and date[0] == 12 and date[1]==31 and date[2]+1 == line[0][2]:
            line.append(data)



def avgbyweek(week, column, rurality):
    calc = 0
    i = 0
    for row in mnhospitaldata2:
        if week == row[1]:
            if isinstance(row[column], float):
                if rurality == True:
                    if row[10] == 'FALSE':
                        calc += row[column]
                        i += 1
                elif rurality == False:
                    if row[10] == 'TRUE':
                        calc += row[column]
                        i += 1
    if i>0:
        return calc/i
    else:
        return None

#calculate & add in bed data from mnhospitaldata2
usedweeks = []

print('Running calculations on hospital data...')
for i in range(len(mnhospitaldata2)):
    week = mnhospitaldata2[i][1]
    if week not in usedweeks:
        usedweeks.append(week)
        ruralicubedsavg = avgbyweek(week, 21, True)
        urbanicubedsavg = avgbyweek(week, 21, False)
        validappend(week, ruralicubedsavg) #avg 7-day staffed icu beds per day reported
        validappend(week, urbanicubedsavg)
        ruralicuoccupancy = avgbyweek(week, 23, True)
        urbanicuoccupancy = avgbyweek(week, 23, False)
        validappend(week, ruralicuoccupancy) #avg 7-day icu patients per day reported
        validappend(week, urbanicuoccupancy)
        ruralinptbeds = avgbyweek(week, 19, True) #avg 7-day num inpt beds per day reported
        urbaninptbeds = avgbyweek(week, 19, False)
        validappend(week, ruralinptbeds)
        validappend(week, urbaninptbeds)

for ch in starcherdataset:
    if isinstance(ch[0], list):
        datestring = '/'.join(map(str,ch[0]))
        ch[0] = datestring

import csv

print('Writing starcherdataset.csv...')
with open('starcherdataset.csv', 'w', newline='') as f:
    writer = csv.writer(f)
    writer.writerows(starcherdataset)

print('Processing complete!')

# print("\nLength of MN Hospital Data:", len(mnhospitaldata))
# print(mnhospitaldata[17086]) #[0] contains labels

# print('\n')
# print('MN Hospital data2:')
# print(mnhospitaldata2[40])

# print('\nStarcher Dataset:')
# print(starcherdataset[0])
# # print(starcherdataset[75])
# # print(starcherdataset[100])
for line in starcherdataset:
    if len(line)!=15:
        print('\nERROR LINE LENGTH:')
        print('\t', line)

# print('\n')
# print('Countyweeks Dataset')
# print(countyweeks[0])