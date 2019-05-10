# ======================================================================
# This is a template dockerfile for working on HackOregon data science 
# projects locally on your laptop in 2019
# usage at a terminal:
# $ docker run -ti -p 12345:12345 karenngcivicsoftware/hackordatasciencetemplate:latest -v ${PWD}:/root/code/
# then the docker container should start.
# after the container starts, you can launch jupyter notebook with either:
# $ ipynb
# or equivalently you can run
# $ jupyter notebook --ip 0.0.0.0 --no-browser --allow-root 
# then navigate to your web browser and copy and paste the url printed out from the terminal command
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
