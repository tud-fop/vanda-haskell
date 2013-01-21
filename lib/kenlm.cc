#include "lm/model.hh"
#include "lm/virtual_interface.hh"
#include <iostream>
#include <string>

using namespace lm::ngram;

extern "C" {

	extern TrieModel* loadModel(char* path) {
		return new TrieModel(path);
	}

	extern float score(TrieModel* model, char* sentence) {
		const SortedVocabulary &vocab = model->GetVocabulary();
		State oldState(model->BeginSentenceState()), newState;
		std::string word;
		std::stringstream ss(sentence);
		float sum = 0;
		while (ss >> word) {
			sum = sum + model->Score(oldState, vocab.Index(word), newState);
			oldState = newState;
		}
		return sum;
	}
	
}

int main(int argc, char* argv[]) {
	TrieModel* model = loadModel(argv[1]);
	printf("%f\n", score(model, argv[2]));
	return 0;
}
