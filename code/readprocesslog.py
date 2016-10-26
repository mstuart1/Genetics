#! /usr/bin/env python

# adapted from latlon_3.py - for use in Chapter 10 PCfB 
# Read in each line of the example file, split it into separate components, and write certain output to a separate file

# Set the input file name
# (The program must be run from within the directory that contains this data file)
#InFileName = './logs/16process.out'
InFileName = raw_input("Enter the path and file name of the log, i.e. ./logs/process65.out: ")

# Open the input file for reading
InFile = open(InFileName, 'r')

# Initialize the counter used to keep track of line numbers
LineNumber = 0

# Open the output file for writing
# Do this *before* the loop, not inside it
OutFileName=InFileName + ".tsv"

OutFile=open(OutFileName,'w') # You can append instead with 'a'

# Loop through each line in the file
for Line in InFile:
        # Skip the header, line # 0
        if LineNumber < 62:
                if LineNumber > 13:
                        # Remove the line ending characters
                        Line=Line.strip('\n')
                        
                        # Separate the line into a list of its tab-delimited components
                        ElementList=Line.split('\t')

                        Barcode   = ElementList[0]
                        Total_Reads = ElementList[1]
                        Lack_RadTag = ElementList[2]  
                        Low_Quality = ElementList[3]
                        Retained = ElementList[4]
                        
                        # Use the % operator to generate a string 
                        # We can use this for output both to the screen and to a file
                        OutputString = "%s\t%s\t%s\t%s\t%s" % \
                           (ElementList[0], ElementList[1], ElementList[2], ElementList[3], ElementList[4])
                           
                        # Can still print to the screen then write to a file
                        
                         print OutputString
                        
                        # Unlike print statements, .write needs a linefeed
                        OutFile.write(OutputString+"\n")  
                
        # Index the counter used to keep track of line numbers
        LineNumber = LineNumber + 1

# After the loop is completed, close the files
InFile.close()
OutFile.close()
