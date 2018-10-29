% CPSC 312 2018 facts - the facts for assignment 4, question 1
% The facts may not be accurate. Consult web page for up-to-date information.
% Copyright David Poole 2018, licenced by CC BY-NC-SA 4.0 http://creativecommons.org/licenses/by-nc-sa/4.0/deed.en

% instructor(Course, Year, Who) is true if Who is the instructor for Course in Year
instructor(cs312,2018,davidp).

% ta(Course, Year, Who) is true if Who is a teaching assistant for Course in Year
ta(cs312,2018,ainaz).
ta(cs312,2018,liran).
ta(cs312,2018,rge).

% name(P,F,L) means person P's first name is F and last name is L
name(davidp, "David", "Poole").
name(ainaz , "Ainaz", "Hajimoradlou").
name(liran, "Liran", "Li").
name(rge, "Rui", "Ge").

% office(P,R) means room R is person P's office
office(davidp,iccs109).

% email(P,E) means E is person P's email address
email(davidp,"poole@cs.ubc.ca").
email(ainaz,"ainaz@cs.ubc.ca").
email(liran,"liran.li@alumni.ubc.ca").
email(rge,"rge@cs.ubc.ca").

% assignment(A, M, D) means assigment A is due at 11:59pm on day D of month M
assignment(as1, september, 13).
assignment(as2, september, 20).
assignment(as3, september, 30).
assignment(as4, november, 1).
assignment(as5, november, 8).

% exam(E, M, D) means exam E is on day D of month M
exam(mid1, september, 24).
exam(mid2, october, 22).
exam(mid3, november, 14).
exam(final, december, 18).

% number_of_classes_before(E,D) is true if there are D classes
% (since the start of term or the previous midterm) before exam E
number_of_classes_before(mid1,8).
number_of_classes_before(mid2,10).
number_of_classes_before(mid3,8).
number_of_classes_before(final,7).

% office_hour(P, D, S, F) means the office hours for person P are are on day D from S to F (on a 24 hour clock).
% This is taken from the course home page
office_hour(davidp, wednesday, 15, 16).
office_hour(ainaz, thursday, 16, 17).
office_hour(ainaz, tuesday, 17, 18).
office_hour(liran, monday, 11, 12).
office_hour(liran, wednesday, 17, 18).
office_hour(rge, thursday, 10, 11).
office_hour(rge, friday, 16, 17).

