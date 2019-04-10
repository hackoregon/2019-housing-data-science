# Purpose
This is meant for use when you are:
1. setting up a GitHub data science project structure locally
2. extracting and reproducing the software setup from `Google Colaboratory` notebook instances or from Amazon `SageMaker`

# Naming convention for Hack Oregon data science github projects 
* `2019-{project-name}-{data-science}`


# Different versions of Data Science docker templates
This contains Dockerfile templates in different flavors for getting started
on the data science parts of a `HackOregon` project. 

1) `master` branch contains basic Python based dependencies 
2) `R` branch contains R-based dependencies 
3) `MLflow-py` for experimental Python workflow that uses `MLflow`
4) others coming soon 


# What the template does:
1. set up a recommended folder structure with `cookercutter`
2. set up library dependencies for extracting documentation as a website
	* `Python`: help set up `Sphinx` for extracting docstring documentation about the APIs 
	* `R`: help set up `KnitR` and `ROxygen2` for extracting the comments from
			different parts of the R code
3. set up testing infrastructure for validating the correctness of the code
	* `Python`: We recommend to use one of the `pytest` or `unittest` frameworks 
	* 
4. Reproduce library setup from Cloud-based notebook instances 
e.g. 
* `AWS SageMaker`
		* [R usage example with KnitR reports](https://rstudio-pubs-static.s3.amazonaws.com/456313_9f8f6ba90b7a4a70a5f8cef7753d2d19.html)
* `Google Cloud Colaboratory`

# Data storage in our public S3 bucket
raw-data = `hacko-data-archive`
clean-data = ?  # in the future

## Storing non-sensitive data to S3 data buckets 
* have a data science manager (or data scientist) of your project  contact Michael to get an AWS account 

## Getting non-sensitive data from S3 data buckets 
```
from sagemaker import get_execution_role

role = get_execution_role()
bucket = 'hacko-data-archieve'
# example data key, change this
data_key = '2018-neighborhood-development/JSON/pdx_bicycle/pdx_bike_counts.csv'

data_location = 's3://{}/{}'.format(bucket, data_key)
output_location = 's3://{}/{}'.format(bucket, data_key)
```

## SageMaker 
We may spin up allow `sagemaker` instances for projects with big compute and / or data needs.
* Naming convention for notebooks instances: 
	* `PROJECTNAME_AUTHOR_NAME`

# Past version of the Docker container template 
https://github.com/hackoregon/data-science-pet-containers

# Using AWS using CLI 
Put your credentials in 
```
~/.aws/credential
```
