# Death Row Inmates study

- Ensure you have git installed, by running `brew install git` in your `terminal`
- Consider installing tessaract native
    - You only need this if you plan on manually labling data from images
- Ensure you can run Rscript in your `terminal`

The data for this study is located on the below URL. Take a look so we can get some questions to answer:
- `https://www.tdcj.texas.gov/death_row/dr_executed_offenders.html`

The questions we aim to answer: ( Ideas welcome, just me doodling )
1. ==Analyze the relationship between how long inmates are on deathrow versus the number of words they speak for the last statement?== ( stop words excluded ) Note: We know the date of when they are received vs the date the are executed.
2. ==Analyze the number of words spoken for the inmates with low levels of education?== ( Some inmates have no school, others 6 years, and rarely any college. Does education play a factor in how much they speak? )
3. ==We can try to extract the tone of the sentance ( sad, happy, remorseful, disappointed, etc. ), and relate it to the number of years spent on deathrow?== ( Not sure if there are any programs out there for R, but I know `grammarly` can do it )



Let's discuss the directories in this project. You can make changes to the structure if you do not like it. Just be mindful on what other changes that constitutes ( specifically, r scripts )

- The `data_extraction` folder contains a scrape of data on the above website.
- The `data_model` folder contains our code for modelling the data we scraped.

To get started, you can go to the `data_model` directory and execute the hello world. If you see no output, something is wrong.

```sh
$ cd data_model
$ Rscript hello-world.r
```

Some basic questions that you will most likely have:

1. How do I label data?
    - Navigate to the `data_extraction` folder.
    - You can run three different R files
        - `fetch_available_executions.r`, `fetch_inmate_info.r`, `fetch_last_statement.r`
        - Example on how to run the last script:
        - The changes will show up in the `data_extraction/inmate_last_statement`
            ```sh
            $ cd data_extraction
            $ r
            $ install.packages("rvest")
            $ install.packages("stringr")
            $ install.packages("tesseract")
            $ install.packages("imager")
            $ install.packages("rapport")
            $ install.packages("properties")
            $ source("fetch_last_statement.r")
            $ downloadInmateLastStatement()
            ```
2. Are we scraping everything from html?
    - Sadly, no. 66% of the data is located in JPG images. If you want to scrape data from images, I'd reccomend the use of tessaract. It is implemented and used in `fetch_inmate_info.r`
3. How accurate is tessaract?
    - Not very accurate in our use, becase the uploaded documents differ in format, terminology, etc. Sometimes, you have to read the entire case to find the date of birth of an inmate.
4. What did I parse out so far?
    - In the last few weeks, I parsed out the date of birth, date received, date of offense, education level, hair color, eye color, native country, native state, gender, and occupation.
5. Is the data correct? Can I verify it?
    - There are 3 verify scripts in the `data_extraction` directory. They are used to sanity check what was parsed out from the website and jpg images, after I've taken a look at it. Like the code example above, you simply import the source for the file you want, and call the validate function. 
    - NOTE: I might have forgotten to use a library import in some of them. Please forgive me if you see an error. If you cannot get past the error, ping me, and I'll help.
6. Did I make any changes to the data?
    - Yes, but nothing major. The department of justice seems to use different formats for the same thing: gray vs grey, delivery man vs delivery driver, construction vs construction worker. I tried to keep the data consistent, so that we refer to the same word.
7. I found a mistake. What do I do?
    - All of the scripts are automated. If you notice that some data is bad, try to fix it, let the teammates know, and push the changes to the repo. ( If you cannot, I can try <3 ) The scripts that we will use to analyze the model always fetch the latest data from our directories, so we should see little to no major impact in code structure after a change is made.