#include <iostream>
#include <vector>
#include <map>
#include <sstream>
using namespace std;

bool is_label(const string &s) {
    for (int i = 0; i < s.length(); ++i) {
        if (s[i] == ':') return true;
        else if (!isalnum(s[i])) return false;
    }
    return false;
}

string get_label(const string &s) {
    for (int i = 0; i < s.length(); ++i) {
        if (s[i] == ':') return s.substr(0, i);
    }
    return "";
}

void replace(string &s, const string &before, const string &after) {
    int pos = 0;
    while(pos = s.find(before, pos), pos != string::npos) {
        s.replace(pos, before.length(), after);
        pos += after.length();
    }
}

string itoa(int i) {
    stringstream ss;
    ss<<i;
    return ss.str();
}

int main() {
    vector<string> input;
    vector<int> line_number;
    map<string, int> labels;
    string line;
    int line_cnt = 0;
    while(getline(cin, line)) {
        input.push_back(line);
        if (is_label(line)) {
            line_number.push_back(line_cnt);
        } else {
            line_number.push_back(line_cnt++);
        }
    }

    for (int i = 0; i < input.size(); ++i) {
        if (is_label(input[i])) {
            for (int j = 0; j < input.size(); ++j) {
                if (i == j) continue;
                replace(input[j], get_label(input[i]), itoa(line_number[i]));
            }
        }
    }

    for (int i = 0; i < input.size(); ++i) {
        if (!is_label(input[i])) cout<<input[i]<<endl;
    }
}
