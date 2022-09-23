USE WORK;
SELECT * FROM shopuser;

DESC shopuser;
INSERT INTO shopuser VALUES('tiger', '홍길동', 25);
INSERT INTO shopuser VALUES('lion', '김삿갓', 30);

DESC shopproduct;
INSERT INTO shopproduct VALUES(1, '냉장고', 1000000);
INSERT INTO shopproduct VALUES(2, '세탁기', 550000);
INSERT INTO shopproduct VALUES(3, 'TV', 1200000);
INSERT INTO shopproduct VALUES(4, '청소기', 200000);
SELECT * FROM shopproduct;


CREATE TABLE shopsale (
uid VARCHAR(10),
pcode int,
scount INT);

DESC shopsale;
INSERT INTO shopsale VALUES('tiger', 2, 1);
INSERT INTO shopsale VALUES('lion', 3, 2);
INSERT INTO shopsale VALUES('lion', 4, 2);
INSERT INTO shopsale values('tiger', 4, 3);
SELECT * FROM shopsale;

UPDATE shopsale SET uid = 'tiger', pcode = 1, scount = 3 WHERE uid = 'tiger'
DROP TABLE shopsale;


/*            JOIN 쿼리(별명 사용)             */
SELECT u.uid, u.uname, s.pcode, u.uage, s.scount  
	FROM shopuser u, shopsale s
	WHERE u.uid = s.uid;

/*         shopproduct 와 shopsale 조인        */
SELECT p.pcode, s.scount, p.pname
	FROM shopproduct as p, shopsale as s
	WHERE p.pcode = s.pcode;


/*    shopuser, shopsale, shopproduct 조인    */
SELECT su.uname, sp.pname, ss.scount
	FROM shopuser AS su, shopproduct AS sp, shopsale AS ss
	WHERE su.uid = ss.uid AND sp.pcode = ss.pcode; 
	
	
	
	
/*  ▣ 김삿갓이 구매한 내역에 대해 성명, 상품명, 수량 조회 */	
/* 없던 열을 새로이 생성할 수 있다 (sp.pprice * ss.scount) */
SELECT su.uname, sp.pname, ss.scount, sp.pprice * ss.scount subtot
	FROM shopuser AS su, shopproduct AS sp, shopsale AS ss
	WHERE su.uid = ss.uid AND sp.pcode = ss.pcode AND su.uname = '김삿갓'; 
	

/* 서브쿼리 : 조회한 결과를 다른 조회구문의 입력 */
/* 세탁기를 구매한 고객들의 모든 이름 */

SELECT * FROM shopsale;

SELECT * FROM shopuser WHERE uid =
	(SELECT * FROM shopsale WHERE pcode =
		(SELECT pcode FROM shopproduct WHERE pname = '세탁기'));

