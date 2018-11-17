import csv


def convert(file_name, predicate_name, string_index):
    arr=[]
    with open('%s.csv' % (file_name), 'rt') as f:
        reader = csv.reader(f)
        for row in reader:
            for i in range(len(row)):
                if i in string_index:
                    row[i]='"%s"' % (row[i])
            arr.append(row)
    arr.pop(0)

    with open('%s.pl' % (file_name), 'w') as f:
        for row in arr:
            line=','.join(row)
            f.write("%s(%s).\n" % (predicate_name, line))

convert('pokemon', 'pokeTypes', [1,11,2,3])