max_vars(5).
max_body(4).
max_clauses(4).

% head
head_pred(e,1).
type(e,(train_type,)).
direction(e,(in,)).

% body

body_pred(car,1).
type(car,(car_type,)).
direction(car,(in,)).

body_pred(shape,1).
type(shape,(shape_type,)).
direction(shape,(in,)).

body_pred(train,1).
type(train,(train_type,)).
direction(train,(in,)).

body_pred(short,1).
type(short,(car_type,)).
direction(short,(in,)).

body_pred(closed,1).
type(closed,(car_type,)).
direction(closed,(in,)).

body_pred(double,1).
type(double,(car_type,)).
direction(double,(in,)).

body_pred(long,1).
type(long,(car_type,)).
direction(long,(in,)).

body_pred(has_car,2).
type(has_car,(train_type,car_type)).
direction(has_car,(in,out)).

body_pred(jagged,1).
type(jagged,(car_type,)).
direction(jagged,(in,)).

body_pred(load,3).
type(load,(car_type,shape_type,element)).
direction(load,(in,in,out)).

body_pred(open_car,1).
type(open_car,(car_type,)).
direction(open_car,(in,)).

body_pred(car_shape,2).
type(car_shape,(car_type,shape_type)).
direction(car_shape,(in,out)).

body_pred(short,1).
type(short,(car_type,)).
direction(short,(in,)).

body_pred(wheels,2).
type(wheels,(car_type,element)).
direction(wheels,(in,out)).
