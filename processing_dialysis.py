#Python version 2.7
#Copyright by Nile Dixon, Tall For Nothing 

import csv
import json
from statistics import mean

data = {}
final_data = {'number_companies' : [], 'state' : [], 'average_rating' : [], 'average_mortality' : []}

#Opens CSV file to prepare data for R script
with open("raw_data.csv","r") as filetoread:
	csvfile = csv.reader(filetoread)
	for row in csvfile:
		#Skip mortality rates that are empty
		if (row[69] == ""):
			pass
		#So that we can skip the lines that cause an issue
		try:
			state = row[9]
			county = row[11]	
			five_star = int(row[4])
			mortality_rate = int(row[69])
			if state not in data:
				data[state] = {}
			if county not in data[state]:
				data[state][county] = {'five_star' : [], 'mortality_rate' : []}
			data[state][county]['five_star'].append(five_star)
			data[state][county]['mortality_rate'].append(mortality_rate)
		except:
			pass

#Put data into Dictionary to format for R Script
for state in data:
	for county in data[state]:
		print str(county)
		number_companies = len(data[state][county]['five_star'])
		average_rating = mean(data[state][county]['five_star'])
		average_mortality = mean(data[state][county]['mortality_rate'])

		final_data['number_companies'].append(number_companies)
		final_data['state'].append(state)
		final_data['average_rating'].append(five_star)
		final_data['average_mortality'].append(average_mortality)

#Saves Dictionary into JSON file for R Script
with open("preprocessed_dialysis.json","w") as filetowrite:
	json.dump(final_data, filetowrite, indent = 4)	
