This file contains additional information about accessing the required BSAS and BPHS datasets.

The datasets are available through the UK Data Service at https://www.ukdataservice.ac.uk/. You must create an account and verify your identity as an academic user in order to obtain access, which may take several business days.

After downloading the datasets, please unzip the files and place them as follows:

- BSAS data should go into a folder named "bsas" within this "replication" folder. Within the "bsas" folder, the data should be available directly as STATA files (e.g. "bsas1983.dta").
- BPHS data should go into a folder named "ukhls" within this "replication" folder. Within the "ukhls" folder, there should be another folder named "UKDA-6614-stata". The file structure inside "UKDA-6614-stata" should remain the same as when the downloaded file was unzipped.

Subsequently, you should run the two R scripts located in this folder, which will theoretically enable you to convert the downloaded data into dataframes that are compatible with the replication. There is no need to set a working directory; the current folder should be used as the working directory for those R scripts.

Once the scripts are run, the data should automatically be placed into the "data" subfolder; if they are not, you should move the files there.
