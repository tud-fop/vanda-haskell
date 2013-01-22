#include "lm/model.hh"
#include "lm/virtual_interface.hh"
#include <iostream>
#include <string>

using namespace lm::ngram;

extern "C" {

	extern TrieModel* loadModel(const char* path) {
		return new TrieModel(path);
	}

	extern float score(const TrieModel* model, const char* sentence) {
		const SortedVocabulary &vocab = model->GetVocabulary();
		State oldState(model->BeginSentenceState());
		State newState;
		std::string word;
		std::stringstream ss(sentence);
		float sum = 0;
		while (ss >> word) {
			sum = sum + model->Score(oldState, vocab.Index(word), newState);
			oldState = newState;
		}
		return sum;
	}

	extern State beginSentenceState(const TrieModel* model) {
		return model->BeginSentenceState();
	}

	extern State nullContextState(const TrieModel* model) {
		return model->NullContextState();
	}

	extern float lookup(const TrieModel* model, const char* sentence, const State* startState) {
		const SortedVocabulary &vocab = model->GetVocabulary();
		State oldState(*startState);
		State newState;
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
