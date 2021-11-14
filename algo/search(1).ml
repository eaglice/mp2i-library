{
 "cells": [
  {
   "cell_type": "code",
   "execution_count": 1,
   "id": "3a4df6ac-c912-4ee7-8205-e5d5d8b726aa",
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/plain": [
       "val recherche_dicho : 'a array -> 'a -> bool = <fun>\n"
      ]
     },
     "execution_count": 1,
     "metadata": {},
     "output_type": "execute_result"
    }
   ],
   "source": [
    "let recherche_dicho t x = \n",
    "    let e = Array.length t -1 in \n",
    "    let (a,b) = ( 0,  e) in\n",
    "    let (d,f) = (ref a, ref b) in\n",
    "            while (!d < !f ) do\n",
    "                let m = (!d + !f)/2 in \n",
    "                if t.(m) < x then\n",
    "                    d := m + 1\n",
    "                else\n",
    "                    f := m; \n",
    "            done;\n",
    "            t.(!d) = x;;"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 2,
   "id": "208edc23-baf2-44bc-895e-0eaa8a7c71f9",
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/plain": [
       "val dicho_rec : 'a array -> 'a -> bool = <fun>\n"
      ]
     },
     "execution_count": 2,
     "metadata": {},
     "output_type": "execute_result"
    }
   ],
   "source": [
    "let dicho_rec t x =\n",
    "    let rec aux i j =\n",
    "    if i > j then false\n",
    "    else let m = (i + j)/2 in \n",
    "        if t.(m) = x then true\n",
    "        else if t.(m) < x then aux (m + 1) j\n",
    "        else aux i (m- 1)\n",
    "    in aux 0 (Array.length t - 1)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 3,
   "id": "1a74b8c5-8f2f-49c9-a011-dd5410094cde",
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/plain": [
       "val tricho_rec : 'a array -> 'a -> bool = <fun>\n"
      ]
     },
     "execution_count": 3,
     "metadata": {},
     "output_type": "execute_result"
    }
   ],
   "source": [
    "let tricho_rec t x =\n",
    "    let rec aux i j =\n",
    "        if i > j then false\n",
    "        else let m1 = (2*i + j + 1 )/ 3 in\n",
    "            let m2 = ( i + 2*j + 2)/3 in\n",
    "            if t.(m1) = x || t.(m2) = x then true\n",
    "            else if x < t.(m1) then aux i (m1-1)\n",
    "            else if x < t.(m2) then aux (m1 + 1) (m2 - 1)\n",
    "            else aux (m2 + 1 ) j in\n",
    "    aux 0 (Array.length t -1)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "id": "efe31134-3b08-46e1-8e7e-b08c069acbc3",
   "metadata": {},
   "outputs": [],
   "source": []
  }
 ],
 "metadata": {
  "kernelspec": {
   "display_name": "OCaml default",
   "language": "OCaml",
   "name": "ocaml-jupyter"
  },
  "language_info": {
   "codemirror_mode": "text/x-ocaml",
   "file_extension": ".ml",
   "mimetype": "text/x-ocaml",
   "name": "OCaml",
   "nbconverter_exporter": null,
   "pygments_lexer": "OCaml",
   "version": "4.08.1"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 5
}
