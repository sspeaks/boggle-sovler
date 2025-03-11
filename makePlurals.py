import inflect
from nltk.corpus import wordnet as wn
import nltk

# Ensure WordNet data is available
nltk.download('wordnet')
nltk.download('omw-1.4')  # Needed for certain language functionalities

def is_plural(word):
    engine = inflect.engine()
    return word == engine.plural(word)

def is_noun(word):
    synsets = wn.synsets(word)
    # Check if any synset has the POS (part of speech) as NOUN
    for synset in synsets:
        if synset.pos() == 'n':
            if synset.lemmas()[0].name() != word:
                return False
            return True
    return False

def main():
    input_file = 'dictionary_keys.txt'
    output_file = 'updated_dictionary_keys.txt'

    words = set()

    with open(input_file, 'r') as file:
        for line in file:
            word = line.strip()
            if word:
                words.add(word)

    engine = inflect.engine()

    for word in list(words):
        if is_noun(word) and not is_plural(word):
            plural_word = engine.plural(word)
            words.add(plural_word)

    sorted_words = sorted(words)

    with open(output_file, 'w') as file:
        for word in sorted_words:
            file.write(word + '\n')

if __name__ == '__main__':
    main()
