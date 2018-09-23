# Project 1: Movies, Genres & Production Companies

## Which movies & production companies are most profitable?

By: Min Yan BEH (mbeh)

Deployed on [shinyapps.io](https://mbeh.shinyapps.io/project1/)

*Note: Pls wait 3-5 seconds for data to load on charts.*

### Notes to Instructor

* This project employs a similar dataset as in HW2 (Movies theme), with the following differences:
  * expanded dataset richness by including more information e.g. production companies involved in every movie
  * significantly improved visualizations to answer interesting analytics questions, such as
    1. Compare genre profiles for different movie production companies (e.g. Warner Bros vs Marvel Studios)
    2. Compare relative performance of production companies e.g Marvel Studios vs DC Comics
    3. Visualize and rank movies which made the biggest profits/losses
  * added more features:
    * melt dataframe which wasn't done previously
    * new input types like radio buttons
    * add complexity to page layout like column/row structure


* Used shinydashboard, like for HW2 due to these preferences:
  * easier to customize dashboard layout to my liking, such as global vs local sidebars
  * server-side rendering abilities: flexdashboard on its own can only render client-side and appears to have slower performance/loading times
  * shinydashboard is more intuitive for me, as someone with a bit of programming background

* Filters for genres/production companies only list a subset of all existing production companies in the dataset (this goes up to the hundreds). This subset has been hardcoded to ensure a more readable user interface.


Here's the shinyapps.io link to [HW2](https://mbeh.shinyapps.io/hw-2/) for your reference.

### Data Source

The dataset was extracted from `movies_metadata.csv` on [Kaggle's "The Movie Dataset"](https://www.kaggle.com/rounakbanik/the-movies-dataset), which was curated by MovieLens. [MovieLens](https://movielens.org) is a non-commercial site that provides movie recommendations for free.

For the purpose of this application, I've only used a subset of this dataset (reduced size from 34MB to 2MB) to include high-budget movies produced in English, and only movies from 1952 onwards. Details on the cleaning pipeline have been documented in the `data/movielens-cleaning-pipeline.ipynb` file in the HW2 project