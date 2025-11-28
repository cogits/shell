const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const ast_module = b.addModule("Ast", .{
        .root_source_file = b.path("Ast/Ast.zig"),
    });

    const command_module = b.createModule(.{
        .root_source_file = b.path("src/Command.zig"),
    });
    command_module.addImport("Ast", ast_module);

    const shell = b.addExecutable(.{
        .name = "shell",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    shell.root_module.addImport("Command", command_module);

    b.installArtifact(shell);

    const run_cmd = b.addRunArtifact(shell);
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // Creates a step for unit testing. This only builds the test executable
    // but does not run it.
    const lib_unit_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("Ast/Ast.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    // Shell execution tests
    const shell_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/test.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    shell_tests.root_module.addImport("Command", command_module);
    const run_shell_tests = b.addRunArtifact(shell_tests);

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
    test_step.dependOn(&run_shell_tests.step);

    // release
    const release_step = b.step("release", "Build for multiple targets to make a release");
    for (targets) |t| {
        const exe = b.addExecutable(.{
            .name = b.fmt("shell-{s}-{s}", .{ @tagName(t.cpu_arch.?), @tagName(t.os_tag.?) }),
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/main.zig"),
                .target = b.resolveTargetQuery(t),
                .optimize = .ReleaseSmall,
                .link_libc = true,
            }),
        });
        exe.root_module.addImport("Command", command_module);

        const target_output = b.addInstallArtifact(exe, .{
            .dest_dir = .{ .override = .{ .custom = "release" } },
        });

        release_step.dependOn(&target_output.step);
    }
}

const targets: []const std.Target.Query = &.{
    .{ .cpu_arch = .x86, .os_tag = .linux },
    .{ .cpu_arch = .x86_64, .os_tag = .linux },
    .{ .cpu_arch = .arm, .os_tag = .linux, .abi = .musleabi },
    .{ .cpu_arch = .aarch64, .os_tag = .linux },
    .{ .cpu_arch = .riscv32, .os_tag = .linux },
    .{ .cpu_arch = .riscv64, .os_tag = .linux },
    .{ .cpu_arch = .s390x, .os_tag = .linux },
    .{ .cpu_arch = .powerpc64le, .os_tag = .linux },
    .{ .cpu_arch = .loongarch64, .os_tag = .linux },

    .{ .cpu_arch = .x86_64, .os_tag = .macos },
    .{ .cpu_arch = .aarch64, .os_tag = .macos },

    .{ .cpu_arch = .x86_64, .os_tag = .freebsd },
    .{ .cpu_arch = .aarch64, .os_tag = .freebsd },
    .{ .cpu_arch = .riscv64, .os_tag = .freebsd },
    .{ .cpu_arch = .powerpc64, .os_tag = .freebsd },
    .{ .cpu_arch = .powerpc64le, .os_tag = .freebsd },

    .{ .cpu_arch = .x86, .os_tag = .netbsd },
    .{ .cpu_arch = .x86_64, .os_tag = .netbsd },
    .{ .cpu_arch = .aarch64, .os_tag = .netbsd },
    .{ .cpu_arch = .arm, .os_tag = .netbsd, .abi = .eabihf },
};
