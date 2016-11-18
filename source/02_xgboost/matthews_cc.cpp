
#include <Rcpp.h>
#include <iostream>
#include <math.h>
#include <algorithm>
#include <iterator>

using namespace Rcpp;

// [[Rcpp::export]]
double calc_mcc(NumericVector preds, LogicalVector labels, double threshold){

	long len = preds.length();
	LogicalVector preds_labels(len), truth_labels(len);
	LogicalVector a_check(len), b_check(len), c_check(len), d_check(len);
	double a,b,c,d;
	double mcc;

	preds_labels = preds > threshold;
	truth_labels = labels == 1;

	a_check = preds_labels * truth_labels;
	b_check = preds_labels * !(truth_labels);
	c_check = !(preds_labels) * truth_labels;
	d_check = !(preds_labels) * !(truth_labels);

	a = std::accumulate(a_check.begin(), a_check.end(), 0);
	b = std::accumulate(b_check.begin(), b_check.end(), 0);
	c = std::accumulate(c_check.begin(), c_check.end(), 0);
	d = std::accumulate(d_check.begin(), d_check.end(), 0);

	mcc = (a*d - b*c) / sqrt((a+b) * (a+c) * (d+b) * (d+c));

	return(mcc);
}

// [[Rcpp::export]]
double search_best_threshold(NumericVector preds, LogicalVector labels, NumericVector thresholds){

	LogicalVector truth_labels(labels.length());
	double best_threshold, best_score;

	best_threshold = 99999;
	best_score = 0;

	for(int i=0; i<thresholds.length(); i++){
		double threshold = thresholds[i];
		double score = calc_mcc(preds, labels, threshold);
		if(best_score < score){
			best_score = score;
			best_threshold = threshold;
		}
	}

	return(best_threshold);
}
