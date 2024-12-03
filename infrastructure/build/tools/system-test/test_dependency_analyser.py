##############################################################################
# (c) Crown copyright 2022 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################
"""
System test of Fortran Dependency Analyser
"""
from pathlib import Path
from subprocess import run
from typing import List

from pytest import fixture


class TestFortranDependencyAnalyser:
    """
    Checks functioning of the dependency analsysis process.
    """
    def __process_under_test(self,
                             tmp_path: Path,
                             tool_dir: Path,
                             dependencies_file: Path,
                             programs_file: Path,
                             source_dir: Path,
                             source_files: List[Path]):
        """
        Performs the analysis process including generating outputs.
        """
        database = tmp_path / 'dependencies.db'

        # Pass each source file to the DependencyAnalyser program.
        #
        for fobject in source_files:
            command = ['python', str(tool_dir / 'DependencyAnalyser'),
                       '-verbose',
                       str(database),
                       str(fobject)]
            process = run(command, cwd=source_dir)
            assert process.returncode == 0

        # Build and check the dependency rules.
        #
        command = ['python', str(tool_dir / 'DependencyRules'),
                   '-verbose',
                   '-database', str(database), '-moduledir', 'modules',
                   '-objectdir', 'objects', str(dependencies_file)]
        process = run(command)
        assert process.returncode == 0

        # Build and check the program rules.
        #
        command = ['python', str(tool_dir / 'ProgramObjects'),
                   '-database', str(database),
                   '-objectdir', 'objects',
                   str(programs_file)]
        process = run(command)
        assert process.returncode == 0

    @fixture(scope='class')
    def test_dir(self):
        return Path(__file__).parent

    @fixture(scope='class')
    def source_dir(self, test_dir: Path):
        return test_dir / 'source'

    @fixture(scope='class')
    def tool_dir(self, test_dir: Path):
        return test_dir.parent

    def test_dependencies(self,
                          source_dir: Path,
                          tool_dir: Path,
                          test_dir: Path,
                          tmp_path: Path):
        """
        Checks the dependency analysis process works.
        """
        # Prepare paths
        #
        dependencies_file = tmp_path / 'dependencies.mk'
        programs_file = tmp_path / 'programs.mk'

        # Build a list of source files
        #
        dirs_to_explore = [source_dir]
        source_files = []
        while dirs_to_explore:
            candidate = dirs_to_explore.pop()
            if candidate.is_dir():
                dirs_to_explore.extend(candidate.iterdir())
            else:
                source_files.append(candidate.relative_to(source_dir))

        # Initial test
        #
        self.__process_under_test(tmp_path,
                                  tool_dir,
                                  dependencies_file,
                                  programs_file,
                                  source_dir,
                                  source_files)
        assert dependencies_file.read_text() \
            == (test_dir / 'expected.dependencies.mk').read_text()
        assert programs_file.read_text() \
            == (test_dir / 'expected.programs.mk').read_text()

        # One file changes but dependencies don't.
        #
        print(source_files[0])
        self.__process_under_test(tmp_path,
                                  tool_dir,
                                  dependencies_file,
                                  programs_file,
                                  source_dir,
                                  [source_files[0]])
        assert dependencies_file.read_text() \
            == (test_dir / 'expected.dependencies.mk').read_text()
        assert programs_file.read_text() \
            == (test_dir / 'expected.programs.mk').read_text()
