import { idmap_empty, idmap_delete, idmap_set } from './messages';

describe('idmap_empty', () => {
    it('should return idmap without keys', () => {
        const map1 = idmap_empty();
        expect(Object.keys(map1).length).toBe(0);
    });
});

describe('idmap_set', () => {
    it('should return new idmap with value', () => {
        const val1 = { id: '1' };

        const map1 = idmap_empty();
        const map2 = idmap_set(map1, val1);
        expect(map2 === map1).toBeFalsy();
        expect(Object.keys(map2)).toContain(val1.id);
        expect(map2[val1.id] === val1).toBeTruthy();
    });

    it('should return same idmap if same value added twice', () => {
        const val1 = { id: '1' };

        const map1 = idmap_set(idmap_empty(), val1);

        const map2 = idmap_set(map1, val1);
        expect(map2 === map1).toBeTruthy();
    });

    it('should return new idmap with value replaced if added new value with same id', () => {
        const val1 = { id: '1' };
        const val2 = { id: '1' };

        const map1 = idmap_set(idmap_empty(), val1);

        const map2 = idmap_set(map1, val2);
        expect(map2 === map1).toBeFalsy();
        expect(Object.keys(map2).length).toBe(1);
        expect(map2[val2.id] === val2).toBeTruthy();
    });
});

describe('idmap_delete', () => {
    it('should return same idmap if id is not found', () => {
        const val1 = { id: '1' };
        const val2 = { id: '2' };

        const map1 = idmap_set(idmap_empty(), val1);

        const map2 = idmap_delete(map1, val2.id);
        expect(map2 === map1).toBeTruthy();
    });

    it('should return new idmap without deleted value', () => {
        const val1 = { id: '1' };
        const val2 = { id: '2' };
        const map1 = idmap_set(idmap_set(idmap_empty(), val1), val2);

        const map2 = idmap_delete(map1, val2.id);
        expect(map2 === map1).toBeFalsy();
        expect(Object.keys(map2).length).toBe(1);
        expect(Object.keys(map2)).toContain(val1.id);
    });
});
