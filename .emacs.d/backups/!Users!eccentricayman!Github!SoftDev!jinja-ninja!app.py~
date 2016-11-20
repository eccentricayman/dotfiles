from flask import Flask, render_template
import random

app = Flask(__name__)

@app.route("/")
def occupationsrender():
    OCUPATIONES = {}
    occupashuns = open("occupations.csv", "rU")
    occupashuns.readline()
    for line in occupashuns:
        if line[0] == "\"":
            #"how obfuscated can i make this next line..."
            # - me half an hour earlier
            #sorry mr. dw
            OCUPATIONES[line[1:line.find("\"", 1, (len(line) + 1))].strip("\n")] = [line[line.find(",", line.find("\"", 1, (len(line) + 1)), (len(line))) + 1:line.find(",", line.find(",", line.find("\"", 1, (len(line) + 1)), (len(line))) + 1, len(line))].strip("\n"), line[line.find("http", line.find(",", line.find(",", line.find("\"", 1, (len(line) + 1)), (len(line))) + 1, len(line)), len(line)):len(line)] ]
        else:
            x = line.split(",")
            OCUPATIONES[x[0].strip("\n")] = [x[1].strip("\n"), x[2].strip("\n")]

    occupashuns.close()
    newoccdict = OCUPATIONES
    del newoccdict['Total']
    rando = random.randint(0, len(newoccdict.keys()));
    return render_template("occupations.html", occudict = OCUPATIONES.items(), randoccu = [newoccdict.keys()[rando], newoccdict.values()[rando][0], newoccdict.values()[rando][1]])

if __name__ == "__main__":
    #app.debug = True
    app.run()
