# Death Row Inmates study

- Ensure you have git installed, by running `brew install git` in your `terminal`
- Consider installing tessaract native
    - You only need this if you plan on manually labling data from images
- Ensure you can run Rscript in your `terminal`

The data for this study is located here
- `https://www.tdcj.texas.gov/death_row/dr_executed_offenders.html`

To get started, let's discuss the directory strcuture.

- The `data_extraction` directory contains a scrape of data on the above website.
- The `data_model` directory contains our code for modelling the data we scraped.

Note: You can make changes to the directory structure. Just be mindful on what other changes that requires ( specifically, r scripts )

To get started, you can go to the `data_model` directory and execute the hello world. If you see no output, something is wrong.

```sh
$ cd data_model
$ Rscript hello-world.r
```

1. How do I manually label data?
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
2. What are the verify scripts?
    - They are used to sanity check what was parsed out from the website and jpg images. Like the example above, you simply import the source for the file you want, and call the validate function. 
    - NOTE: I might have forgotten to use a library import in some of them. Please forgive me if you see an error. If you cannot get past the error, ping me, and I'll help.