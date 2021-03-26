# Langbase Database Schema Converter

This is a tool created in Shiny R to convert Langbase's old database schema (which originally had a sentences, words, and sources table for each language) into a more tidy version of itself. 
The tool takes a JSON file that can be retrieved from exporting the database in PHPMyAdmin, and it outputs four tables (words, sentences, sources, and users). 
These tables can be downloaded as CSVs and then reuploaded into PHPMyAdmin. The website is located at: https://rockfordmankini.shinyapps.io/LangbaseSentenceConverter/

***

## app.R

The Shiny R webapp. This is where all of the code is. None of the supporting files such as the database or the files that maintain the connection to ShinyApps.io for security reasons.

© Rockford Mankini, 2021