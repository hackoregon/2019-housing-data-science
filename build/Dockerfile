# ======================================================================
# This is a template dockerfile for working on HackOregon data science 
# projects locally on your laptop in 2019
# usage: docker run -ti -p 12345:8888 :latest 
# within the container, use jupyter notebook with:
# $ jupyter notebook --ip 0.0.0.0 --no-browser --allow-root -v ${PWD}:/code/DataScience
# then navigate to your web browser and go to the url:
# 127.0.0.1:12345
# to see the notebook
# ======================================================================
FROM continuumio/miniconda3:latest
MAINTAINER Karen Ng <karen.yyng@gmail.com>

# install dependencies for psycopg2
RUN apt-get update && \
    apt-get install -y libpq-dev && \
    rm -rf /var/lib/apt/lists/*

RUN pip install --no-cache-dir --upgrade \
    psycopg2-binary==2.8.1 \
    pyarrow==0.11.1 \
    pandas==0.24.2 \
    bokeh==1.0.4 \
    pytest==4.1.1 \ 
    sphinx==1.8.5 \
    jupyter==1.0.0 \
    matplotlib \
    xlrd==1.2.0 \
    statsmodels==0.9.0 \
    s3fs==0.2.0 \
    recommonmark==0.5.0 \
    sphinx-markdown-tables \
    cookiecutter

RUN echo 'alias ipynb="jupyter notebook --port 12345 --no-browser --ip 0.0.0.0 --allow-root"' >> /root/.bashrc
WORKDIR /root/project
