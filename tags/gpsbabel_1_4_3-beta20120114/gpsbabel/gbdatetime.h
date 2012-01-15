// Copyright ME.   

#include <QDateTime>

// As this code began in C, we have several hundred places that set and
// read creation_time as a time_t.  Provide some operator overloads to make
// that less painful.
class gbDateTime : public QDateTime {
public:
  operator const time_t() const {
    return this->toTime_t();
  }
  const time_t& operator=(const time_t& t) {
    this->setTime_t(t);
  }
};


